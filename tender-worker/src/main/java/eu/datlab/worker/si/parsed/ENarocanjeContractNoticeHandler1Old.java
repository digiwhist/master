package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Handler for parsing old forms "EU 2 - SL" and "EU 5 - SL".
 */
final class ENarocanjeContractNoticeHandler1Old extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler1Old() {}

    /**
     * Parses form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonFormInTableAttributes(tender, form);

        Element sectionIV21 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.2.1", form);
        if (sectionIV21 != null && sectionIV21.text().contains("IV.1.2")) {
            // We have wrong section, because the number of desired section "IV.2.1" is mentioned in "IV.1.2".
            // The problem is probably only in:
            // https://www.enarocanje.si/Obrazci/?id_obrazec=39769
            sectionIV21 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.select(String.format(
                            ENarocanjeTenderFormInTableUtils.SECTION_FIRST_ELEMENT_SELECTOR_PATTERN, "IV.2.1"), form)
                            .last(),
                    null);
        }

        String procedureType = StringUtils.removeDotsAtTheEnd(
            ENarocanjeTenderFormInTableUtils.getSectionContent("IV.1.1", form, Arrays.asList("Vrsta postopka")));

        tender
            .setBuyers(ENarocanjeTenderFormInTableUtils.parseBuyerType(tender.getBuyers(), form))
            .setTitle(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.1", form))
            .setSupplyType(parseSupplyType("II.1.2", form))
            .setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.5", form))
            .setCpvs(parseTenderCpvs("II.1.6", form))
            .setHasLots(parseIfTenderHasLots("II.1.8", form))
            .setAreVariantsAccepted(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                    ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.9", form))))
            .setNationalProcedureType(procedureType)
            .setProcedureType(procedureType)
            .setSelectionMethod(StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(sectionIV21, null, 6)))
            .setAwardCriteria(parseTenderAwardCriteria(sectionIV21))
            .setBidDeadline(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.3.4", form, Arrays.asList(
                "Rok za sprejemanje ponudb ali prijav za sodelovanje:", "Rok za prejem ponudb ali prijav za"
                    + " sodelovanje")))
            .addEligibleBidLanguage(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.3.6", form))
            .setFundings(parseFundings(form))
            .setHasOptions(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("II.2.2", form, Arrays.asList(":", "Opcije")))))
            .setEstimatedPrice(parsePrice(ENarocanjeTenderFormInTableUtils.getSectionContent("II.2.1", form)))
            .setIsElectronicAuction(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.2.2", form))))
            .setAwardDeadline(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.3.7", form, Arrays.asList("Do:")));


        assert tender.getPublications().get(0).getIsIncluded().equals(Boolean.toString(true));
        tender.getPublications().get(0)
                .setBuyerAssignedId(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.3.1", form, Arrays.asList(
                        "Referenčna številka dokumenta, ki jo je določil naročnik")));

        return tender;
    }

    /**
     * Parses list of fundings.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return non-empty list of fundings or null
     */
    private static List<ParsedFunding> parseFundings(final Element form) {
        final String isEuFund = ENarocanjeTenderFormInTableUtils.getSectionContent("VI.2", form, Arrays.asList(
                        "NAROČILO SE NANAŠA NA PROJEKT IN/ALI PROGRAM, KI SE FINANCIRA IZ SREDSTEV SKUPNOSTI"));
        return isEuFund == null ? null : Collections.singletonList(new ParsedFunding()
                .setIsEuFund(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(isEuFund))));
    }

}
