package eu.digiwhist.worker.ch.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.ch.parsed.SimapTenderParser.parseAnythingRightUnder;
import static eu.digiwhist.worker.ch.parsed.SimapTenderParser.selectTextUnderHeader;

/**
 * Created by michalriha on 20/03/2017.
 */
public final class SimapTenderAwardHandler {

    /**
     * Private constructor.
     */
    private SimapTenderAwardHandler() {
    }

    /**
     * Main method to parse award forms.
     *
     * @param parsedTender parsed tender to fill
     * @param document     document to parse from
     *
     * @return ParsedTender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        return parsedTender
                .addPublications(parseAdditionalPublications(document))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(selectTextUnderHeader("2.6", document)))
                .setIsOnBehalfOf(
                        parseAnythingRightUnder("span:containsOwn(Service demandeur/Entité adjudicatrice)", document))
                .addBuyer(new ParsedBody()
                        .setName(parseAnythingRightUnder(new String[]{
                                "span:containsOwn(Service organisateur/Entité organisatrice)",
                                "span:containsOwn(Bedarfsstelle/Vergabestelle)"}, document))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseAnythingRightUnder(new String[]{
                                        "span:containsOwn(Service organisateur/Entité organisatrice)",
                                        "span:containsOwn(Beschaffungsstelle/Organisator)"}, document)))
                        .setBuyerType(selectTextUnderHeader("1.2", document)))
                .setNationalProcedureType(selectTextUnderHeader("1.3", document))
                .setSupplyType(selectTextUnderHeader("1.4", document))
                .setIsCoveredByGpa(selectTextUnderHeader("1.5", document))
                .setTitle(selectTextUnderHeader("2.1", document))
                .setCpvs(parseCPVs(document))
                .setAwardDecisionDate(selectTextUnderHeader("4.2", document))
                .addLot(parseLot(document));
    }

    /**
     * Parse lot.
     *
     * @param document document to parse from
     *
     * @return ParsedTenderLot
     */
    private static ParsedTenderLot parseLot(final Document document) {
        return new ParsedTenderLot()
                .setBidsCount(selectTextUnderHeader("4.3", document))
                .addBid(new ParsedBid()
                        .setIsWinning(String.valueOf(true))
                        .addBidder(new ParsedBody()
                                .setName(parseAnythingRightUnder(
                                        "h4:containsOwn(Liste des adjudicataires) + dl span:containsOwn" +
                                                "(Nom)", document))
                                .setAddress(new ParsedAddress()
                                        .setRawAddress(parseAnythingRightUnder(
                                                "h4:containsOwn(Liste des adjudicataires) + dl " +
                                                        "span:containsOwn(Nom)", document))))
                        .setPrice(new ParsedPrice()
                                .setNetAmount(parseAnythingRightUnder("h4:containsOwn(Liste des adjudicataires) + dl " +
                                        "span:containsOwn(Prix)", document))));
    }

    /**
     * Parse additional publications.
     *
     * @param document document to parse from
     *
     * @return List<ParsedPublication> or null
     */
    private static List<ParsedPublication> parseAdditionalPublications(final Document document) {
        Elements publicationIds = document.select("h1:containsOwn(Appel d'offres) + dl > dd:containsOwn(VD)");

        if (publicationIds == null || publicationIds.isEmpty()) {
            publicationIds = document.select("h1:containsOwn(Appel d'offres) + dl > dd:containsOwn(GR)");
        }

        if (publicationIds == null || publicationIds.isEmpty()) {
            return null;
        }

        final List<ParsedPublication> parsedPublications = new ArrayList<>();

        for (Element publicationId : publicationIds) {
            parsedPublications.add(new ParsedPublication()
                    .setSourceId(publicationId.text())
                    .setIsIncluded(false)
                    .setSource(PublicationSources.CH_SIMAP));
        }

        return parsedPublications;
    }

    /**
     * Parse cpvs.
     *
     * @param document document to parse from
     *
     * @return List<ParsedCPV> or null
     */
    private static List<ParsedCPV> parseCPVs(final Document document) {
        final Elements cpvs = document.select("td:contains(CPV:) + td");

        if (cpvs == null || cpvs.isEmpty()) {
            return null;
        }

        final List<ParsedCPV> parsedCpvs = new ArrayList<>();

        for (Element cpv : cpvs) {
            parsedCpvs.add(new ParsedCPV()
                    .setCode(cpv.text()));
        }

        return parsedCpvs;
    }
}
