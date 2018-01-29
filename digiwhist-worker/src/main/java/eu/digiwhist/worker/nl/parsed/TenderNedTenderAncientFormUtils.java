package eu.digiwhist.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utilities for TenderNed ancient forms.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderAncientFormUtils {
    private static final Logger logger = LoggerFactory.getLogger(TenderNedTenderAncientFormUtils.class);

    // selectors of subsection contents in ancient forms (mostly but not always - most contract notices has the ancient
    // format until 01-06-2015 but some contract notice from 25-08-2011 has new format)
    static final String ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionI'] ~ h4:has(span:containsOwn(I.1)) + p";
    static final String ANCIENT_SUBSECTION_I_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionI'] ~ h4:has(span:containsOwn(I.2)) + p";
    static final String ANCIENT_SUBSECTION_II_1_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.1)) + p";
    static final String ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.2)) + p";
    static final String ANCIENT_SUBSECTION_II_1_3_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.3)) + p";
    static final String ANCIENT_SUBSECTION_II_1_4_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.4)) + p";
    static final String ANCIENT_SUBSECTION_II_1_5_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.5)) + p";
    static final String ANCIENT_SUBSECTION_II_1_6_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.6)) + p";
    static final String ANCIENT_SUBSECTION_II_1_7_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.7)) + p";
    static final String ANCIENT_SUBSECTION_II_1_9_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.1.9)) + p";
    static final String ANCIENT_SUBSECTION_II_2_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.2.2)) + p";
    static final String ANCIENT_SUBSECTION_II_3_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionII'] ~ h4:has(span:containsOwn(II.3)) + p";
    static final String ANCIENT_SUBSECTION_III_1_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIII'] ~ h4:has(span:containsOwn(III.1.1)) + p";
    static final String ANCIENT_SUBSECTION_III_2_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIII'] ~ h4:has(span:containsOwn(III.2.1)) + p";
    static final String ANCIENT_SUBSECTION_III_2_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIII'] ~ h4:has(span:containsOwn(III.2.2)) + p";
    static final String ANCIENT_SUBSECTION_III_2_3_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIII'] ~ h4:has(span:containsOwn(III.2.3)) + p";
    static final String ANCIENT_SUBSECTION_IV_1_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.1.1)) + p";
    static final String ANCIENT_SUBSECTION_IV_2_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.2.1)) + p";
    static final String ANCIENT_SUBSECTION_IV_2_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.2.2)) + p";
    static final String ANCIENT_SUBSECTION_IV_3_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.3.1)) + p";
    static final String ANCIENT_SUBSECTION_IV_3_4_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.3.4)) + p";
    static final String ANCIENT_SUBSECTION_IV_3_6_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionIV'] ~ h4:has(span:containsOwn(IV.3.6)) + p";
    static final String ANCIENT_SUBSECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionV'] + p";
    static final String ANCIENT_SUBSECTION_VI_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.1)) + p";
    static final String ANCIENT_SUBSECTION_VI_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.2)) + p";
    static final String ANCIENT_SUBSECTION_VI_3_1_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.3.1.1)) + p";
    static final String ANCIENT_SUBSECTION_VI_3_1_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.3.1.2)) + p";
    static final String ANCIENT_SUBSECTION_VI_4_1_1_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.4.1.1)) + p";
    static final String ANCIENT_SUBSECTION_VI_4_1_2_CONTENT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionVI'] ~ h4:has(span:containsOwn(VI.4.1.2)) + p";
    static final String ANCIENT_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR =
            "h3[id='detail-publicatie:linkSectionB'] + p";

    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderAncientFormUtils() {
    }

    /**
     * Parse buyer phone from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseBuyerPhone(final Element form) {
        final String phoneAndFax = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR,
                "Telefoon:");
        if (phoneAndFax == null) {
            return null;
        }

        // Comma separates phone and fax. When fax is not presented, comma can be last character (sometimes is and
        // sometimes is not).
        return phoneAndFax.contains(",")
                ? phoneAndFax.substring(0, phoneAndFax.indexOf(','))
                : phoneAndFax;
    }

    /**
     * Parse buyer URL from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseBuyerUrl(final Element form) {
        String url = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, "Internetadres(sen):");
        if (url == null) {
            return null;
        } else if (url.contains("\"")) {
            // the string on row is something like
            // "Internetadres(sen):  <a href="http://www.heerhugowaard.nl">www.heerhugowaard.nl</a>"
            assert url.contains("\">");
            return url.substring(url.indexOf('"') + 1, url.indexOf("\">"));
        } else {
            // the string on row is something like "Internetadres(sen):  www.montferland.info"
            return url;
        }
    }

    /**
     * Parse second buyer from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return buyer body or Null
     */
    static ParsedBody parseSecondTenderBuyer(final Element form) {
        Element subsection = JsoupUtils.selectFirst(ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, form);
        String[] subsectionRows = subsection.html().split("<br>");

        Integer secondBuyerIndex = null;
        for (int i = 0; i < subsectionRows.length; ++i) {
            String subsectionRow = subsectionRows[i];
            if (subsectionRow.contains("Nadere inlichtingen zijn te verkrijgen op het volgende adres:")) {
                secondBuyerIndex = i + 1;
                break;
            }
        }
        // return null if information about the second tender buyer do not exist or they refer to the first tender
        // buyer. The reference can be on two different rows:
        // - https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        // 73d33fe9123821d64468ddc32cf7f04a/pageId/D909A/huidigemenu/aankondigingen/cid/953576/cvp/join
        // - https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        // 4e7330d5816ea52819c9a61de1974699/pageId/D909A/huidigemenu/aankondigingen/cid/1124934/cvp/join
        final String isTheSameAsFirstBuyerString = "Zelfde adres als voor het/de hierboven vermelde contactpunt(en)";
        if (secondBuyerIndex == null
                || subsectionRows[secondBuyerIndex - 1].contains(isTheSameAsFirstBuyerString)
                || subsectionRows[secondBuyerIndex].contains(isTheSameAsFirstBuyerString)) {
            return null;
        }

        ParsedBody secondTenderBuyer = new ParsedBody();
        final String contactNameFormString = "Ter attentie van:";
        final String phoneNameFormString = "Telefoon:";
        final String emailNameFormString = "E-mail:";
        // iterate over rows until we are in second tender buyer part
        while (secondBuyerIndex < subsectionRows.length
                && !subsectionRows[secondBuyerIndex].contains("Bestek en aanvullende documenten (zoals documenten " +
                "voor een concurrentiegerichte dialoog en een dynamisch aankoopsysteem) zijn verkrijgbaar op:")) {
            final String subsectionRow = subsectionRows[secondBuyerIndex];
            if (subsectionRow.startsWith(contactNameFormString)) {
                secondTenderBuyer.setContactName(subsectionRow.substring(contactNameFormString.length()).trim());
            } else if (subsectionRow.startsWith(phoneNameFormString)) {
                // Comma separates phone and fax. When fax is not presented, comma can be last character (sometimes is
                // and sometimes is not).
                return secondTenderBuyer.setPhone(subsectionRow.contains(",")
                        ? subsectionRow.substring(phoneNameFormString.length(), subsectionRow.indexOf(',')).trim()
                        : subsectionRow.substring(phoneNameFormString.length()).trim());
            } else if (subsectionRow.startsWith(emailNameFormString)) {
                final String webFormString = ", Internetadres (URL):";
                // comma separates email and URL when URL is presented, otherwise only email without comma is presented
                if (subsectionRow.contains(webFormString)) {
                    secondTenderBuyer.setEmail(
                            subsectionRow.substring(emailNameFormString.length(), subsectionRow.indexOf(',')).trim());
                    secondTenderBuyer.setAddress(new ParsedAddress()
                            .setUrl(subsectionRow.substring(
                                    subsectionRow.indexOf(webFormString) + webFormString.length()).trim()));
                } else {
                    secondTenderBuyer.setEmail(
                            subsectionRow.substring(emailNameFormString.length()).trim());
                }
            }
            ++secondBuyerIndex;
        }
        return secondTenderBuyer;
    }

    /**
     * Parse supply type of tender from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseTenderSupplyType(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR, "Type:");
    }

    /**
     * Parse tender on behalf of bodies from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of parsed on behalf of bodies or Null
     */
    static List<ParsedBody> parseTenderOnBehalfOf(final Element form) {
        String isTenderOnBehalfOfFilledText = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_2_CONTENT_SELECTOR,
                "De aanbestedende dienst koopt aan namens andere aanbestedende diensten :");
        if (isTenderOnBehalfOfFilledText == null) {
            // sometimes space is not before colon
            isTenderOnBehalfOfFilledText = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_2_CONTENT_SELECTOR,
                    "De aanbestedende dienst koopt aan namens andere aanbestedende diensten:");
        }

        if (isTenderOnBehalfOfFilledText == null || isTenderOnBehalfOfFilledText.equals("neen")) {
            return null;
        }

        assert isTenderOnBehalfOfFilledText.equals("ja");

        // todo: implement when we have annotation
        return Arrays.asList(new ParsedBody());
    }

    /**
     * Parse tender title from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseTenderTitle(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_1_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse nuts of address of implementation from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseAddressOfImplementationNuts(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR, "NUTS-code:");
    }

    /**
     * Parse if tender is awarded as superior framework agreement.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseIsTenderFrameworkAgreement(final Element form) {
        // isTenderFrameworkAgreement is true when subsection text is:
        // 1) "De opstelling van een raamovereenkomst" (it is the text maybe for contract notice)
        //  - https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        //    05980cf91318f48123474eef34f78bec/pageId/D909A/huidigemenu/aankondigingen/cid/1392646/cvp/join
        // 2) "De sluiting van een raamovereenkomst" (it is the text maybe for contract award)
        //   - https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        //    caa1925324cec1cccf390d78039ad76b/pageId/D909A/huidigemenu/aankondigingen/cid/881508/cvp/join
        final String text = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_3_CONTENT_SELECTOR, 0);
        return BooleanUtils.toStringTrueFalse(text != null
                && (text.equals("De opstelling van een raamovereenkomst")
                    || text.equals("De sluiting van een raamovereenkomst")));
    }

    /**
     * Parse tender description from document.
     *
     * @param form
     *         document to be parsed
     * @param subsectionContentSelector
     *         selector which gets content containing tender description
     *
     * @return String or Null
     */
    static String parseTenderDescription(final Element form, final String subsectionContentSelector) {
        return ParserUtils.getFromContent(form, subsectionContentSelector, 0);
    }

    /**
     * Parse tender CPVs from document.
     *
     * @param form
     *         document to be parsed
     * @param subsectionContentSelector
     *         selector which gets content containing CPVs
     *
     * @return list of parsed CPVs or Null
     */
    static List<ParsedCPV> parseTenderCpvs(final Element form, final String subsectionContentSelector) {
        List<ParsedCPV> cpvs = new ArrayList<>();

        Element subsection = JsoupUtils.selectFirst(subsectionContentSelector, form);
        String[] subsectionRows = subsection.html().split("<br>");

        // CPVs are filled by two different ways:
        // 1) https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        //    e87b17be7e05c33c65114796b840811d/pageId/D909A/huidigemenu/aankondigingen/cid/420728/cvp/join
        // 2) https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        //    0d491e1d75187a67343b938ec7bb42ae/pageId/D909A/huidigemenu/aankondigingen/cid/351913/cvp/join
        if (subsectionRows[0].contains("Hoofdopdracht")) {
            final String cpvTitle = "Hoofdcategorieën:";

            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(subsectionRows[1].substring(cpvTitle.length(), subsectionRows[1].indexOf('(')).trim()));

            for (int i = 2; i < subsectionRows.length; ++i) {
                String subsectionRow = subsectionRows[i];
                if (subsectionRow.startsWith(cpvTitle)) {
                    cpvs.add(new ParsedCPV()
                            .setIsMain(Boolean.FALSE.toString())
                            .setCode(subsectionRow.substring(cpvTitle.length(), subsectionRow.indexOf('(')).trim()));
                }
            }
        } else {
            if (subsectionRows.length != 1) {
                logger.error("CPVs are on two or more rows -> refactor the parsing.");
            }
            // each CPV has eight digits
            Pattern p = Pattern.compile("\\d{8}");
            Matcher m = p.matcher(subsectionRows[0]);
            // first CPV code is main
            if (m.find()) {
                cpvs.add(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(m.group()));
            }
            while (m.find()) {
                cpvs.add(new ParsedCPV()
                        .setIsMain(Boolean.FALSE.toString())
                        .setCode(m.group()));
            }
        }

        return cpvs;
    }

    /**
     * Parse tender national procedure type value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseTenderNationalProcedureType(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_1_1_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse award criteria from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return award criteria or Null
     */
    static List<ParsedAwardCriterion> parseAwardCriteria(final Element form) {        
        Element subsection = JsoupUtils.selectFirst(ANCIENT_SUBSECTION_IV_2_1_CONTENT_SELECTOR, form);
        if (subsection == null) {
            return null;
        }

        // attempt to find criteria in subsection content
        Matcher m = Pattern.compile("(?im)^Criterium: (?<name>.+)$(.*\n)*?^WeegFactor: (?<weight>.+)$")
            .matcher(subsection.html().replace("<br>", "\n"));

        List<ParsedAwardCriterion> criteria = new ArrayList<>();        
        // annotation only says that we have to parse award criteria name if subsection IV.2.1 has one row filled
        if (!m.find()) {
            criteria.add(new ParsedAwardCriterion().setName(subsection.html().split("<br>")[0].trim()));
        } else {
            do {
                criteria.add(new ParsedAwardCriterion().setName(m.group("name")).setWeight(m.group("weight")));
            } while (m.find());
        }

        return criteria.isEmpty() ? null : criteria;
    }

    /**
     * Parse if tender is electronic auction from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseIfTenderIsElectronicAuction(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_2_2_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse tender buyer assigned id value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseTenderBuyerAssignedId(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_3_1_CONTENT_SELECTOR, 0);
    }
    
    /**
     * @param form
     *      document to be parsed
     * @return first buyer
     */
    static ParsedBody parseFirstTenderBuyer(final Element form) {
        Element subsection = JsoupUtils.selectFirst(ANCIENT_SUBSECTION_I_2_CONTENT_SELECTOR, form);

        return new ParsedBody()
            .setName(ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, 0))
            .addBodyId(TenderNedTenderFormUtils.parseBodyIdentifier(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, 1)))
            .setAddress(new ParsedAddress()
                .setRawAddress(ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, 2))
                .setUrl(TenderNedTenderAncientFormUtils.parseBuyerUrl(form)))
            .setContactName(ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR,
                "Ter attentie van:"))
            .setPhone(TenderNedTenderAncientFormUtils.parseBuyerPhone(form))
            .setEmail(ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, "E-mail:"))
            .addMainActivity(subsection != null ? subsection.childNode(2).toString() : null)
            .setBuyerType(subsection != null ? subsection.childNode(0).toString() : null);
    }

    /**
     * @param form
     *      parsed form
     * @return selection method
     */
    static String parseSelectionMethod(final Element form) {
        Element subsection = JsoupUtils.selectFirst(ANCIENT_SUBSECTION_IV_2_1_CONTENT_SELECTOR, form);
        if (subsection == null) {
            return null;
        }
        
        return subsection.html().split("<br>")[0].trim();
    }
}
