package eu.datlab.worker.cz.parsed;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

/**
 * Handler for Tender Cancellation Notice on Vestnik.
 */
abstract class VestnikContractCancellationHandler {
    /**
     * Parses Tender Cancellation Notice specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page
     *
     * @return updated tender object with data parsed from Tender Cancellation Notice form
     */
    static ParsedTender parseCommonContractCancellationAttributes(final ParsedTender tender,
            final Document form) {
        tender.setIsWholeTenderCancelled(VestnikTenderParserUtils.getCheckedInputValue(form, "FormItems\\.JdeO_I_6_4"))
                .setCancellationReason(
                        VestnikTenderParserUtils.getTextAfterLabel(form, "FormItems_StrucnyDuvodZruseni_I_6_5"))
                .setLots(parseLots(form));

        return tender;
    }

    /**
     * Parses all the cancelled lots from contract cancellation notice.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of all cancelled lots or empty list if no lots specified in the form
     */
    private static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsSections = getLotsSections(form);
        for (int i = 0; i < lotsSections.size(); i++) {
            lots.add(parseLot(lotsSections.get(i), i + 1));
        }
        return lots;
    }

    /**
     * Finds tender lots (attachments to form 51) in source HTML page and returns list of appropriate forms (one for
     * each
     * lot/attachment).
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of cancelled lot forms or empty list if no lots specified
     */
    private static Elements getLotsSections(final Document form) {
        return form.select("div.sub-content:has(div.iform-name:contains(INFORMACE O ČÁSTECH))");
    }

    /**
     * Parses cancelled tender lot from given lot form (HTML fragment).
     *
     * @param lotSection
     *         parsed tender lot section
     * @param lotPosition
     *         order in which the lot appears on the source page
     *
     * @return parsed tender lot
     */
    private static ParsedTenderLot parseLot(final Element lotSection, final int lotPosition) {
        return new ParsedTenderLot().setPositionOnPage(Integer.toString(lotPosition))
                .setLotNumber(VestnikTenderParserUtils.getFieldValue(lotSection, "AttItems\\.CastZakazkyC_0_0"))
                .setTitle(VestnikTenderParserUtils.getFieldValue(lotSection, "AttItems\\.Nazev_0_0"))
                .setDescription(VestnikTenderParserUtils.getTextAfterLabel(lotSection, "AttItems_StrucnyPopis_1"))
                .setCancellationReason(
                        VestnikTenderParserUtils.getTextAfterLabel(lotSection, "AttItems_StrucnyDuvodZruseni_3"))
                .setCpvs(parseLotCpvCodes(lotSection));
    }

    /**
     * Parses CPV codes for contract notice lot.
     *
     * @param lotSection
     *         parsed tender lot form
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseLotCpvCodes(final Element lotSection) {
        final List<ParsedCPV> cpvs = new ArrayList<>();
        // main CPV
        cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(VestnikTenderParserUtils.getFieldValue(lotSection, "AttItems\\.HlavniSlovnikHp_2")));

        // other CPVs
        cpvs.addAll(lotSection.select("div.iform-field > input[name~=AttItems\\.HlavniSlovnikDp(1|2|3|4)_2]")
                .stream()
                .filter(cpvDetail -> !cpvDetail.attr("value").isEmpty())
                .map(cpvDetail -> new ParsedCPV().setIsMain(Boolean.FALSE.toString())
                        .setCode(cpvDetail.attr("value")))
                .collect(Collectors.toList()));
        return cpvs;
    }
}
