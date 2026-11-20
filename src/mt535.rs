//! MT535 (Statement of Holdings) parser for HIWPD.
//!
//! Banks answer HKWPD with a HIWPD segment whose depot statement is an MT535
//! document in a binary data element (`@len@…`). The fields used here are
//! the ones German banks fill (and python-fints' `MT535_Miniparser` reads):
//!
//! ```text
//! :16R:FIN                              one position
//! :35B:ISIN DE0005140008                ISIN, then optional /DE/<WKN>, then name lines
//! /DE/514000
//! DEUTSCHE BANK AG NAMENS-AKTIEN O.N.
//! :90B::MRKT//ACTU/EUR12,345            price per unit (or :90A::MRKT//PRCT/101,5 in percent)
//! :94B::PRIC//LMAR/XETR                 exchange
//! :98A::PRIC//20261119                  price date (or :98C:: with time)
//! :93B::AGGR//UNIT/10,                  quantity (UNIT = pieces, FAMT = nominal)
//! :19A::HOLD//EUR123,45                 market value (N before the currency: negative)
//! :16S:FIN
//! ```
//!
//! `:97A::SAFE//<blz>/<depot>` in the GENL block names the depot.

use std::str::FromStr;

use chrono::NaiveDate;
use rust_decimal::Decimal;

use crate::types::{Currency, Isin, SecurityHolding, Wkn};

/// Decode the bytes (ISO 8859-1, as FinTS uses) and parse every position.
pub fn parse(bytes: &[u8]) -> Vec<SecurityHolding> {
    let text: String = bytes.iter().map(|&b| b as char).collect();
    parse_str(&text)
}

/// One `:TAG:value` field, continuation lines joined with `\n`.
fn fields(text: &str) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = Vec::new();
    for line in text.split(['\r', '\n']).filter(|l| !l.is_empty()) {
        if let Some((tag, value)) = tag_of(line) {
            out.push((tag.to_string(), value.to_string()));
        } else if line == "-" {
            continue;
        } else if let Some(last) = out.last_mut() {
            last.1.push('\n');
            last.1.push_str(line);
        }
    }
    out
}

/// `:35B:rest` → `("35B", "rest")`: two digits, an optional letter.
fn tag_of(line: &str) -> Option<(&str, &str)> {
    let rest = line.strip_prefix(':')?;
    let end = rest.find(':')?;
    let tag = &rest[..end];
    let ok = (tag.len() == 2 || tag.len() == 3)
        && tag[..2].bytes().all(|b| b.is_ascii_digit())
        && tag[2..].bytes().all(|b| b.is_ascii_uppercase());
    ok.then(|| (tag, &rest[end + 1..]))
}

pub fn parse_str(text: &str) -> Vec<SecurityHolding> {
    let mut holdings = Vec::new();
    let mut depot_id: Option<String> = None;
    let mut current: Option<SecurityHolding> = None;
    // Nesting below FIN (SUBBAL etc.): only FIN-level fields count.
    let mut depth_in_fin = 0usize;
    let mut percent_price = false;

    for (tag, value) in fields(text) {
        match (tag.as_str(), value.as_str()) {
            ("16R", "FIN") => {
                current = Some(empty_holding(depot_id.clone()));
                depth_in_fin = 0;
                percent_price = false;
            }
            ("16S", "FIN") => {
                if let Some(mut h) = current.take() {
                    if h.market_value.is_none() {
                        h.market_value = h.price.map(|p| {
                            if percent_price { h.quantity * p / Decimal::from(100) } else { h.quantity * p }
                        });
                        if h.market_value_currency.is_none() {
                            h.market_value_currency = h.price_currency.clone();
                        }
                    }
                    if h.isin.is_some() || h.wkn.is_some() || !h.name.is_empty() {
                        holdings.push(h);
                    }
                }
            }
            ("16R", _) if current.is_some() => depth_in_fin += 1,
            ("16S", _) if current.is_some() => depth_in_fin = depth_in_fin.saturating_sub(1),
            ("97A", v) if current.is_none() => {
                if let Some(id) = v.strip_prefix(":SAFE//") {
                    depot_id = Some(id.trim().to_string());
                }
            }
            (_, _) if depth_in_fin > 0 => {}
            ("35B", v) => {
                if let Some(h) = current.as_mut() {
                    read_35b(h, v);
                }
            }
            ("90A", v) | ("90B", v) => {
                if let (Some(h), Some(rest)) = (current.as_mut(), v.strip_prefix(":MRKT//")) {
                    if let Some(p) = rest.strip_prefix("PRCT/") {
                        h.price = amount(p);
                        percent_price = true;
                    } else if let Some(p) = rest.strip_prefix("ACTU/").or_else(|| rest.strip_prefix("DISC/")) {
                        let (cur, num) = split_currency(p);
                        h.price = amount(num);
                        h.price_currency = cur.map(Currency::new);
                    }
                }
            }
            ("94B", v) => {
                if let (Some(h), Some(rest)) = (current.as_mut(), v.strip_prefix(":PRIC//")) {
                    let place = rest.rsplit('/').next().unwrap_or(rest).trim();
                    if !place.is_empty() {
                        h.exchange = Some(place.to_string());
                    }
                }
            }
            ("98A", v) | ("98C", v) => {
                if let (Some(h), Some(rest)) = (current.as_mut(), v.strip_prefix(":PRIC//")) {
                    h.price_date = rest.get(..8).and_then(|d| NaiveDate::parse_from_str(d, "%Y%m%d").ok());
                }
            }
            ("93B", v) => {
                if let (Some(h), Some(rest)) = (current.as_mut(), v.strip_prefix(":AGGR//")) {
                    let num = rest
                        .strip_prefix("UNIT/")
                        .or_else(|| rest.strip_prefix("FAMT/"))
                        .unwrap_or(rest);
                    if let Some(q) = amount(num.trim_start_matches('N')) {
                        h.quantity = if num.starts_with('N') { -q } else { q };
                    }
                }
            }
            ("19A", v) => {
                if let (Some(h), Some(rest)) = (current.as_mut(), v.strip_prefix(":HOLD//")) {
                    let (negative, rest) = match rest.strip_prefix('N') {
                        Some(r) => (true, r),
                        None => (false, rest),
                    };
                    let (cur, num) = split_currency(rest);
                    if let Some(a) = amount(num) {
                        h.market_value = Some(if negative { -a } else { a });
                        h.market_value_currency = cur.map(Currency::new);
                    }
                }
            }
            _ => {}
        }
    }
    holdings
}

fn empty_holding(depot_id: Option<String>) -> SecurityHolding {
    SecurityHolding {
        isin: None,
        wkn: None,
        name: String::new(),
        quantity: Decimal::ZERO,
        price: None,
        price_currency: None,
        price_date: None,
        market_value: None,
        market_value_currency: None,
        acquisition_value: None,
        profit_loss: None,
        exchange: None,
        depot_id,
        raw: serde_json::json!({ "source": "mt535" }),
    }
}

/// `ISIN DE0005140008\n/DE/514000\nNAME LINE 1\nNAME LINE 2`
fn read_35b(h: &mut SecurityHolding, v: &str) {
    let mut name = Vec::new();
    for (i, line) in v.lines().enumerate() {
        let line = line.trim();
        if i == 0 {
            if let Some(isin) = line.strip_prefix("ISIN ") {
                h.isin = Some(Isin::new(isin.trim()));
                continue;
            }
        }
        if let Some(rest) = line.strip_prefix("/DE/") {
            h.wkn = Some(Wkn::new(rest.trim()));
        } else if !line.is_empty() {
            name.push(line);
        }
    }
    h.name = name.join(" ");
}

/// `EUR12,345` → (`Some("EUR")`, `"12,345"`); a bare number has no currency.
fn split_currency(s: &str) -> (Option<&str>, &str) {
    if s.len() > 3 && s[..3].bytes().all(|b| b.is_ascii_uppercase()) {
        (Some(&s[..3]), &s[3..])
    } else {
        (None, s)
    }
}

/// SWIFT amounts use a decimal comma and may end in it (`10,`).
fn amount(s: &str) -> Option<Decimal> {
    let s = s.trim().replace(',', ".");
    let s = s.strip_suffix('.').unwrap_or(&s);
    Decimal::from_str(s).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "\r\n:16R:GENL\r\n:28E:1/ONLY\r\n:20C::SEME//NONREF\r\n:23G:NEWM\r\n\
:98A::PREP//20261120\r\n:98A::STAT//20261120\r\n:22F::STTY//CUST\r\n:97A::SAFE//20050550/1234567\r\n\
:17B::ACTI//Y\r\n:16S:GENL\r\n\
:16R:FIN\r\n:35B:ISIN DE0005140008\r\n/DE/514000\r\nDEUTSCHE BANK AG\r\nNAMENS-AKTIEN O.N.\r\n\
:90B::MRKT//ACTU/EUR12,345\r\n:94B::PRIC//LMAR/XETR\r\n:98A::PRIC//20261119\r\n\
:93B::AGGR//UNIT/10,\r\n:16R:SUBBAL\r\n:93C::TAVI//UNIT/AVAI/10,\r\n:16S:SUBBAL\r\n\
:19A::HOLD//EUR123,45\r\n:70E::HOLD//1STK++++20200101+\r\n:16S:FIN\r\n\
:16R:FIN\r\n:35B:ISIN DE0001102580\r\nBUNDESREP.DEUTSCHLAND ANL.\r\n\
:90A::MRKT//PRCT/101,5\r\n:98C::PRIC//20261119173000\r\n:93B::AGGR//FAMT/1000,\r\n:16S:FIN\r\n\
:16R:ADDINFO\r\n:19A::HOLP//EUR1138,45\r\n:16S:ADDINFO\r\n-";

    #[test]
    fn parses_positions() {
        let h = parse_str(SAMPLE);
        assert_eq!(h.len(), 2);

        let a = &h[0];
        assert_eq!(a.isin.as_ref().unwrap().as_str(), "DE0005140008");
        assert_eq!(a.wkn.as_ref().unwrap().as_str(), "514000");
        assert_eq!(a.name, "DEUTSCHE BANK AG NAMENS-AKTIEN O.N.");
        assert_eq!(a.quantity, Decimal::from(10));
        assert_eq!(a.price, Some(Decimal::from_str("12.345").unwrap()));
        assert_eq!(a.price_currency.as_ref().unwrap().as_str(), "EUR");
        assert_eq!(a.price_date, NaiveDate::from_ymd_opt(2026, 11, 19));
        assert_eq!(a.exchange.as_deref(), Some("XETR"));
        assert_eq!(a.market_value, Some(Decimal::from_str("123.45").unwrap()));
        assert_eq!(a.depot_id.as_deref(), Some("20050550/1234567"));

        let b = &h[1];
        assert_eq!(b.name, "BUNDESREP.DEUTSCHLAND ANL.");
        assert_eq!(b.quantity, Decimal::from(1000));
        // Percent of nominal: 1000 * 101.5 / 100.
        assert_eq!(b.market_value, Some(Decimal::from_str("1015").unwrap()));
        assert_eq!(b.price_date, NaiveDate::from_ymd_opt(2026, 11, 19));
    }

    #[test]
    fn subbal_quantities_do_not_override_the_aggregate() {
        let h = parse_str(SAMPLE);
        assert_eq!(h[0].quantity, Decimal::from(10));
    }

    #[test]
    fn negative_values() {
        let h = parse_str(":16R:FIN\n:35B:ISIN XX0000000001\nX\n:93B::AGGR//UNIT/N5,\n:19A::HOLD//NEUR10,\n:16S:FIN");
        assert_eq!(h[0].quantity, Decimal::from(-5));
        assert_eq!(h[0].market_value, Some(Decimal::from(-10)));
    }

    #[test]
    fn latin1_names_decode() {
        let mut bytes = b":16R:FIN\n:35B:ISIN XX0000000001\nM".to_vec();
        bytes.push(0xDC); // Ü in ISO 8859-1
        bytes.extend_from_slice(b"NCHENER RE\n:16S:FIN");
        assert_eq!(parse(&bytes)[0].name, "MÜNCHENER RE");
    }

    #[test]
    fn empty_statement() {
        assert!(parse_str(":16R:GENL\n:16S:GENL\n-").is_empty());
    }
}
