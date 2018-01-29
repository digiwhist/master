package eu.digiwhist.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_8_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_9_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_3_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_III_1_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_IV_3_4_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_IV_3_5_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_VI_2_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_VI_3_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR;
import org.jsoup.select.Elements;

/**
 * Parser for TenderNed old contract notice form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderOldContractNoticeHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderOldContractNoticeHandler() {
    }

    /**
     * Parse method for TenderNed old contract notice form from specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return ParsedTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Element form) {
        TenderNedTenderOldAndNewFormUtils.parseCommonAttributes(parsedTender, form);

        parsedTender.getBuyers().get(0).getAddress()
            .setUrl(TenderNedTenderOldAndNewFormUtils.parseOldBuyerUrl(form));

        parsedTender.getBuyers().get(0)
            .setBuyerType(TenderNedTenderOldAndNewFormUtils.parseOldTenderBuyerType(form))
            .setMainActivities(parseBuyerMainActivities(form));

        parsedTender
            .setDocumentsLocation(new ParsedAddress()
                .setUrl(TenderNedTenderOldAndNewFormUtils.parseOldTenderDocumentsUrl(form)))
            .setAddressOfImplementation(TenderNedTenderOldAndNewFormUtils.parseOldTenderAddressOfImplementation(form))
            .setIsFrameworkAgreement(TenderNedTenderOldAndNewFormUtils.parseOldTenderIsFrameworkAgreement(form))
            .setCpvs(TenderNedTenderOldAndNewFormUtils.parseOldTenderCpvs(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR, form))))
            .setHasLots(TenderNedTenderOldAndNewFormUtils.parseIfTenderHasLots(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_8_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_9_TITLE_SELECTOR, form))))
            .setEstimatedStartDate(ParserUtils.getFromContent(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_III_1_TITLE_SELECTOR, form)), "p", "Aanvang:"))
            .setEstimatedCompletionDate(ParserUtils.getFromContent(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_III_1_TITLE_SELECTOR, form)), "p", "Voltooiing:"))
            .setBidDeadline(TenderNedTenderOldAndNewFormUtils.parseTenderBidDeadline(
                ParserUtils.getSubsectionOfElements(
                    JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_3_4_TITLE_SELECTOR, form),
                    JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_3_5_TITLE_SELECTOR, form))))
            .addFunding(TenderNedTenderOldAndNewFormUtils.parseTenderFunding(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_VI_2_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_VI_3_TITLE_SELECTOR, form))))
            .setLots(parseTenderLots(form))
            .addPublications(TenderNedTenderOldAndNewFormUtils.parseOldTenderPreviousPublicationsInTed(form))
            .setSupplyType(TenderNedTenderOldAndNewFormUtils.parseTenderSupplyType(form))
            .setBuyerAssignedId(TenderNedTenderOldAndNewFormUtils.parseBuyerAssignedId(form));

        return parsedTender;
    }

    /**
     * Parse buyer main activity value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return non-empty list of activities or null
     */
    private static List<String> parseBuyerMainActivities(final Element form) {
        Elements nodes = JsoupUtils.select("ul > li", ParserUtils.getSubsectionOfElements(
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR, form),
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR, form)));

        if (nodes == null || nodes.isEmpty()) {
            return null;
        }

        List<String> activities = new ArrayList<>();
        nodes.forEach(n -> {
            activities.add(n.text());
        });

        return activities;
    }

    /**
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of lots or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element form) {
        final String sectionFooterClassName = "section-footer";

        Element lotElement = JsoupUtils.selectFirst(OLD_AND_NEW_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR, form);
        if (lotElement == null) {
            return null;
        }

        List<ParsedTenderLot> lots = new ArrayList<>();
        final String lotNumberInLotFormTitleText = "Perceel nr";
        do {
            String lotFormTitle = JsoupUtils.selectText("span", lotElement);
            assert lotFormTitle.startsWith(lotNumberInLotFormTitleText);
            ParsedTenderLot lot = new ParsedTenderLot();

            final int colonIndex = lotFormTitle.indexOf(':');
            assert colonIndex == lotFormTitle.length() - 1;
            final String lotNumber = lotFormTitle.substring(lotNumberInLotFormTitleText.length(), colonIndex).trim();
            if (!lotNumber.equals("-")) {
                lot.setLotNumber(lotNumber);
            }
            if (!lotElement.ownText().equals("-")) {
                lot.setTitle(lotElement.ownText());
            }

            lotElement = lotElement.nextElementSibling();
            // subsection 1
            if (lotElement.text().startsWith("B.1")) {
                lotElement = lotElement.nextElementSibling();
                String lotDescription = lotElement.ownText().trim();
                if (!lotDescription.isEmpty() && !lotDescription.equals("-")) {
                    lot.setDescription(lotDescription);
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection 2
            if (lotElement.text().startsWith("B.2")) {
                final Element subsection2StartElement = lotElement;
                do {
                    lotElement = lotElement.nextElementSibling();
                } while (lotElement != null && !lotElement.nodeName().equals("h5") // next subsection
                        && !lotElement.text().contains(lotNumberInLotFormTitleText)
                        && !lotElement.className().equals(sectionFooterClassName));
                Element subsection2 = ParserUtils.getSubsectionOfElements(subsection2StartElement, lotElement);
                Element mainTaskElement = JsoupUtils.selectFirst("dl", subsection2);
                if (mainTaskElement != null) {
                    String cpv = JsoupUtils.selectText("dl > dt:containsOwn(Hoofdcategorieën:) + dd", mainTaskElement);
                    if (cpv != null) {
                        lot.addCpv(new ParsedCPV()
                                .setIsMain(Boolean.TRUE.toString())
                                .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
                    }

                    assert JsoupUtils.select("dl > dt:containsOwn(Subcategorieën:) + dd", mainTaskElement).size() <= 1;
                    cpv = JsoupUtils.selectText("dl > dt:containsOwn(Subcategorieën:) + dd", mainTaskElement);
                    if (cpv != null && !cpv.equals("-")) {
                        lot.addCpv(new ParsedCPV()
                                .setIsMain(Boolean.FALSE.toString())
                                .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
                    }
                }
            }

            lots.add(lot);

            // move to the first element of next lot or to the end element of lots
            while (lotElement != null && !lotElement.text().contains(lotNumberInLotFormTitleText)
                    && !lotElement.className().equals(sectionFooterClassName)) {
                lotElement = lotElement.nextElementSibling();
            }

            if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
                break;
            }
        } while (true);
        return lots;
    }

}
