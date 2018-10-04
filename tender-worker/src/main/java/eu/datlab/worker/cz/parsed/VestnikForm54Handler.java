package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form 54 - Framework agreement contract awards.
 */
public final class VestnikForm54Handler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikForm54Handler() {
        throw new AssertionError();
    }

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {

        // set is framework agreement vs is dps
        String isFrameworkAgreement = VestnikTenderParserUtils.getCheckedInputValue(form,
                "FormItems\\.OznameniSeTykaRamcoveSmlouvy_0_0");
        if (isFrameworkAgreement != null) {
            boolean isFA = isFrameworkAgreement.equalsIgnoreCase("true");
            tender.setIsFrameworkAgreement(Boolean.toString(isFA));
            tender.setIsDps(Boolean.toString(!isFA));
        }

        // parse lots
        tender.setLots(parseLots(form));

        return tender;
    }

    /**
     * Parses all the lots from contract award notice.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsSections = getLotsSections(form);
        for (int i = 0; i < lotsSections.size(); i++) {
            Element lotSection = lotsSections.get(i);
            ParsedTenderLot lot = parseLotWithWinningBid(lotSection, i + 1);
            lots.add(lot);
        }
        return lots;
    }

    /**
     * Parses lots sections (ODDIL 4) from the form.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return sections for all the lots
     */
    static Elements getLotsSections(final Document form) {
        return form.select(String.format("div.section:has(div.iform-subsection:matches(^%s))", "ODDÍL 4:.*"));
    }

    /**
     * Parses tender lot from given lot form (HTML fragment).
     *
     * @param lotSection
     *         parsed tender lot section
     * @param lotPosition
     *         order in which the lot appears on the source page
     *
     * @return parsed tender lot
     */
    static ParsedTenderLot parseLotWithWinningBid(final Element lotSection, final int lotPosition) {
        return new ParsedTenderLot().setPositionOnPage(Integer.toString(lotPosition))
                .setContractNumber(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.EvidencniCisloCastiZakazkyPrideleneZadavatelem_IV"))
                .setTitle(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.NazevZakazkyZadaneNaZakladeRamcoveSmlouvy_IV"))
                .setAwardDecisionDate(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.DatumZadaniZakazky_IV"))
                .setBidsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.PocetObdrzenychNabidek_IV"))
                .addBid(new ParsedBid().setIsWinning(Boolean.TRUE.toString())
                        .addBidder(VestnikTenderParserUtils.parseBody(lotSection))
                        .setPrice(new ParsedPrice().setCurrency("CZK")
                                .setNetAmount(VestnikTenderParserUtils.getFieldValue(lotSection,
                                        "AttModels.*AttItems.KonecnaHodnotaZakazky_IV"))));
    }
}
