package eu.datlab.worker.ch.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import static eu.datlab.worker.ch.parsed.SimapTenderParser.parseAnythingRightUnder;
import static eu.datlab.worker.ch.parsed.SimapTenderParser.selectTextUnderHeader;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by michalriha on 20/03/2017.
 */
public final class SimapTenderNoticeHandler {

    /**
     * Private constructor.
     */
    private SimapTenderNoticeHandler() {
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
                        .setName(SimapTenderParser.parseAnythingRightUnder(new String[]{
                                "span:containsOwn(Service organisateur/Entité organisatrice)",
                                "span:containsOwn(Bedarfsstelle/Vergabestelle)"}, document))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseAnythingRightUnder(new String[]{
                                        "span:containsOwn(Service organisateur/Entité organisatrice)",
                                        "span:containsOwn(Beschaffungsstelle/Organisator)"}, document)))
                        .setBuyerType(selectTextUnderHeader("1.6", document)))
                .setEnquiryDeadline(selectTextUnderHeader("1.3", document))
                .setBidDeadline(parseAnythingRightUnder("h3:containsOwn(1.4) + dl span:containsOwn(Date)", document))
                .setNationalProcedureType(selectTextUnderHeader("1.7", document))
                .setSupplyType(selectTextUnderHeader("1.8", document))
                .setIsCoveredByGpa(selectTextUnderHeader("1.9", document))
                .setTitle(selectTextUnderHeader("2.2", document))
                .setCpvs(parseCPVs(document))
                .setDescription(selectTextUnderHeader("2.5", document))
                .setAreVariantsAccepted(selectTextUnderHeader("2.8", document))
                .setEligibilityCriteria(selectTextUnderHeader("3.1", document))
                .setDeposits(selectTextUnderHeader("3.2", document))
                .addAwardCriterion(new ParsedAwardCriterion()
                        .setName(selectTextUnderHeader("3.9", document)))
                .setDocumentsPrice(new ParsedPrice()
                        .setNetAmount(selectTextUnderHeader("3.10", document)))
                .addEligibleBidLanguage(selectTextUnderHeader("3.11", document))
                .setAwardDeadline(selectTextUnderHeader("3.12", document))
                .setDocumentsLocation(new ParsedAddress()
                        .setRawAddress(selectTextUnderHeader("3.13", document)))
                .setAppealBodyName(selectTextUnderHeader("4.7", document));
    }

    /**
     * Parse additional publications.
     *
     * @param document document to parse from
     *
     * @return List<ParsedPublication> or null
     */
    private static List<ParsedPublication> parseAdditionalPublications(final Document document) {
        Elements publicationIds = document.select("h1:containsOwn(Appel d\\'offres) + dl > dd:containsOwn(VD)");

        if (publicationIds == null || publicationIds.isEmpty()) {
            publicationIds = document.select("h1:containsOwn(Appel d\\'offres) + dl > dd:containsOwn(GR)");
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
