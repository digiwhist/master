package eu.datlab.worker.pl.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.Arrays;
import java.util.Optional;

/**
 * Parser for contract award notice form specific data.
 *
 * @author Tomas Mrazek
 */
public final class UZPContractNoticeHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private UZPContractNoticeHandler() {
        throw new AssertionError();
    }

    /**
     * @param document
     *         xml document
     * @param machineReadableUrl
     *      machine readable URL of included publication
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final Document document, final String machineReadableUrl) {

        String publYear = JsoupUtils.selectText("bzp_rok", document);
        String publSourceId = JsoupUtils.selectText("bzp_poz", document);
        ParsedPublication publication = null;
        if (publYear != null || publSourceId != null) {
            publication = new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setPublicationDate(publYear)
                .setSourceId(publSourceId);
        }

        return Arrays.asList(UZPTenderParserUtils.parseCommonFormData(document, machineReadableUrl)
            .addPublication(publication)
            .setAreVariantsAccepted(Optional.ofNullable(UZPTenderParserUtils.isEnabled("czy_wariant", document))
                .map(String::valueOf).orElse(null))
            //tag with same name is also in the lot section (czas_dni)
            .setEstimatedDurationInDays(JsoupUtils.selectText(":root > czas_dni", document))
            //tag with same name is also in the lot section (czas_mies)
            .setEstimatedDurationInMonths(JsoupUtils.selectText(":root > czas_mies", document))
            //tag with same name is also in the lot section (data_roz)
            .setEstimatedStartDate(JsoupUtils.selectText(":root > data_roz", document))
            //tag with same name is also in the lot section (data_zak)
            .setEstimatedCompletionDate(JsoupUtils.selectText(":root > data_zak", document))
            .setDeposits(JsoupUtils.selectCombinedText("wadium, zaliczka", document))
            .setTechnicalRequirements(parseTechnicalRequirements(document))
            .setPersonalRequirements(JsoupUtils.selectText("zdolne", document))
            .setEconomicRequirements(parseEconomicalRequirements(document))
            .setBidDeadline(JsoupUtils.selectText(":root > termin_konk", document))
            .setAwardCriteria(UZPTenderParserUtils.parseAwardCriteria(document))
            .setIsElectronicAuction(Optional.ofNullable(UZPTenderParserUtils.isEnabled("czy_aukcja", document))
                .map(String::valueOf).orElse(null))
            .setDocumentsLocation(parseDocumentsLocation(document))
            .setSpecificationsProvider(parseSpecificationsProvider(document))
            .setLots(parseLots(document))
            .setBidsRecipient(parseBidsRecipient(document))
            .setEnvisagedCandidatesCount(JsoupUtils.selectText("liczba_wyk", document))
            .setEligibilityCriteria(JsoupUtils.selectCombinedText("opis_war, inf_osw", document)));
    }

    /**
     * Parses bid recipient from tven document.
     *
     * @param document
     *          parsed document
     * @return bid recipient or null
     */
    private static ParsedBody parseBidsRecipient(final Document document) {
        String address = JsoupUtils.selectText("miejsce_skl, miejsce, miejsce_konk", document);
        if (address == null) {
            return null;
        }
        
        return new ParsedBody().setAddress(new ParsedAddress().setRawAddress(address));
    }

    /**
     * Parses documents location.
     *
     * @param document
     *      parsed document
     * @return documents location or null
     */
    private static ParsedAddress parseDocumentsLocation(final Document document) {
        String address = JsoupUtils.selectText("spec_war, miejsce_odb", document);
        if (address == null) {
            return null;
        }
        
        return new ParsedAddress().setRawAddress(address);
    }

    /**
     * Parses specifications provider from the given document.
     *
     * @param document
     *      parsed document
     * @return specifications provider or null
     */
    private static ParsedBody parseSpecificationsProvider(final Document document) {
        String url = JsoupUtils.selectText("spec_www", document);
        if (url == null) {
            return null;
        }

        return new ParsedBody().setAddress(new ParsedAddress().setUrl(url));
    }

    /**
     * Parses technical requirements.
     *
     * @param document
     *         document
     *
     * @return technical requiremenets
     */
    private static String parseTechnicalRequirements(final Element document) {
        final Elements elements = new Elements();

        elements.addAll(JsoupUtils.select("wiedza, potencjal, wymagania", document));
        elements.addAll(JsoupUtils.selectNumberedElements(
                (number) -> "oświadczenie_wykluczenia_".concat(number.toString()), document));
        elements.addAll(JsoupUtils.selectNumberedElements(
                (number) -> "oświadczenie_potwierdzenia_".concat(number.toString()), document));
        elements.addAll(JsoupUtils.selectNumberedElements(
            (number) -> "dok_potw_".concat(number.toString()), document));

        return elements.text();
    }

    /**
     * Parses economical requirements.
     *
     * @param document
     *         document
     *
     * @return economical requirements
     */
    private static String parseEconomicalRequirements(final Element document) {
        final Elements elements = new Elements();
        if (JsoupUtils.exists("sytuacja", document)) {
            elements.add(JsoupUtils.selectFirst("sytuacja", document));
        }
        elements.addAll(JsoupUtils.selectNumberedElements(
            (number) -> "dok_podm_zag_".concat(number.toString()), document));

        return elements.text();
    }

    /**
     * Parses tender lots.
     *
     * @param document
     *         document
     * @return list of parsed lots
     */
    private static List<ParsedTenderLot> parseLots(final Element document) {
        final Elements lotNodes = JsoupUtils.select("czesci > *", document);
        if (lotNodes == null || lotNodes.isEmpty()) {
            return null;
        }

        int positionOnPage = 0;
        final List<ParsedTenderLot> lots = new ArrayList<>();
        for (Element node : lotNodes) {
            lots.add(new ParsedTenderLot()
                .setPositionOnPage(String.valueOf(positionOnPage))
                .setTitle(JsoupUtils.selectText("nazwa", node))
                .setDescription(JsoupUtils.selectText("opis", node))
                .setLotNumber(JsoupUtils.selectText("nr_czesci", node))
                .setCpvs(UZPTenderParserUtils.parseCPVs(node))
                .setEstimatedDurationInDays(JsoupUtils.selectText("czas_dni", node))
                .setEstimatedDurationInMonths(JsoupUtils.selectText("czas_mies", node))
                .setEstimatedStartDate(JsoupUtils.selectText("data_roz", node))
                .setEstimatedCompletionDate(JsoupUtils.selectText("data_zak", node))
                .setAwardCriteria(UZPTenderParserUtils.parseAwardCriteria(node)));

            positionOnPage++;
        }

        return lots;
    }
}
