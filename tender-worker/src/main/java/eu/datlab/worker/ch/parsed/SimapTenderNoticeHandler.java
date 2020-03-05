package eu.datlab.worker.ch.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import static eu.datlab.worker.ch.parsed.SimapTenderParser.parseAnythingRightUnder;
import static eu.datlab.worker.ch.parsed.SimapTenderParser.selectTextUnderHeader;

import java.util.ArrayList;
import java.util.Arrays;
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
                        .setRawAddress(selectTextUnderHeader("Lieu de la fourniture, Ort der Ausführung", document)))
                .setIsOnBehalfOf(
                        parseAnythingRightUnder("span:containsOwn(Service demandeur/Entité adjudicatrice)", document))
                .addBuyer(new ParsedBody()
                        .setName(SimapTenderParser.parseAnythingRightUnder(new String[]{
                                "span:containsOwn(Service organisateur/Entité organisatrice)",
                                "span:containsOwn(Bedarfsstelle/Vergabestelle)"}, document))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseBuyerAddress(document)))
                        .setEmail(parseBuyerEmail(document))
                        .setBuyerType(selectTextUnderHeader("1.6", document)))
                .setEnquiryDeadline(selectTextUnderHeader("1.3", document))
                .setBidDeadline(parseBidDeadline(document))
                .setEstimatedStartDate(parseEstimatedStartDate(document))
                .setEstimatedCompletionDate(parseEstimatedCompletionDate(document))
                .setNationalProcedureType(selectTextUnderHeader("1.7", document))
                .setSupplyType(selectTextUnderHeader("1.8", document))
                .setIsCoveredByGpa(selectTextUnderHeader("1.9", document))
                .setTitle(selectTextUnderHeader("2.2", document))
                .setCpvs(parseCPVs(document))
                .setDescription(selectTextUnderHeader("Detaillierter Produktebeschrieb, " +
                        "Detaillierter Projektbeschrieb, 2.5, 2.6", document))
                .setAreVariantsAccepted(selectTextUnderHeader("2.8", document))
                .setEligibilityCriteria(selectTextUnderHeader("3.1", document))
                .setDeposits(selectTextUnderHeader("3.2", document))
                .addAwardCriteria(parseAwardCriteria(document))
                .setDocumentsPrice(new ParsedPrice()
                        .setNetAmount(JsoupUtils.selectOwnText("dd:containsOwn(Kosten)", document)))
                .addEligibleBidLanguage(selectTextUnderHeader("Sprachen für Angebote, 3.11", document))
                .setAwardDeadline(selectTextUnderHeader("Gültigkeit des Angebotes, 3.12", document))
                .setDocumentsLocation(new ParsedAddress()
                        .setRawAddress(selectTextUnderHeader("Bezugsquelle für Ausschreibungsunterlagen, 3.13", document)))
                .setAppealBodyName(selectTextUnderHeader("4.7", document))
                .setLots(parseLots(document));
    }


    /**
     * Parses bid deadline.
     * @param document document to be parsed
     * @return parsed bid deadline
     */
    private static String parseBidDeadline(final Element document){
        Element parentElem = JsoupUtils.selectFirst("h3:containsOwn(1.4) + dl", document);
        if(parentElem == null || parentElem.text().isEmpty()){
            return null;
        }
        String deadline = "";
        for(String part: parentElem.text().split(" ")){
            if(part.matches(".*[0-9][0-9]\\.[0-9][0-9]\\.[0-9]*.*")){
                deadline += part;
            } else if(!deadline.isEmpty() && part.matches("[0-9]*:[0-9]*.?")){
                deadline += " " + part;
            }
        }
        return deadline.replaceAll("[^\\.:[0-9] ]", "");
    }


    /**
     * Parses buyer email.
     * @param document document to be parsed
     * @return parsed buyer email
     */
    private static String parseBuyerEmail(final Element document){
        Element addressHeader = JsoupUtils.selectFirst("span:containsOwn(Beschaffungsstelle/Organisator), " +
                "span:containsOwn(Service organisateur/Entité organisatrice)", document);
        if(addressHeader == null){
            return null;
        }
        String address = addressHeader.parent().ownText();
        if(address.contains("E-Mail")){
            String[] splitAddress = address.split("E-Mail");
            if(splitAddress.length > 1 && splitAddress[1].length() > 3){
                return splitAddress[1].replace(":", "").replace(" ", "");
            }
            Element a = addressHeader.parent().selectFirst("a");
            if(a != null){
                return a.ownText().replace(":", "").replace(" ", "");
            }
        }
        return null;
    }

    /**
     * Parses buyer address.
     * @param document document to be parsed
     * @return parsed buyer address
     */
    private static String parseBuyerAddress(final Element document){
        String address = parseAnythingRightUnder(new String[]{
                "span:containsOwn(Beschaffungsstelle/Organisator)",
                "span:containsOwn(Service organisateur/Entité organisatrice)"}, document);
        if(address == null){
            return null;
        }
        return address.split(", E-Mail")[0].split(", E-mail")[0].split("email")[0];
    }

    /**
     * Parses estimated start date.
     * @param document document to be parsed
     * @return parsed estimated start date
     */
    private static String parseEstimatedStartDate(final Element document){
        Element datesElem = JsoupUtils.selectFirst("h3:containsOwn(Ausführungstermin) + dl", document);
        if(datesElem == null){
            datesElem = JsoupUtils.selectFirst("h3:containsOwn(Laufzeit des Vertrags, der Rahmenvereinbarung " +
                    "oder des dynamischen Beschaffungssystems) + dl", document);
        }
        if(datesElem == null){
            return null;
        }
        String date = datesElem.selectFirst("dd").ownText();
        for(String part: date.split(" ")){
            if(part.matches(".*[0-9]*\\.[0-9]*\\.[0-9]*.*")){
                return part.replaceAll("[^\\.[0-9]]", "");
            }
        }
        return null;
    }

    /**
     * Parses estimated completion date.
     * @param document document to be parsed
     * @return parsed estimated completion date
     */
    private static String parseEstimatedCompletionDate(final Element document){
        Element datesElem = JsoupUtils.selectFirst("h3:containsOwn(Ausführungstermin) + dl", document);
        if(datesElem == null){
            datesElem = JsoupUtils.selectFirst("h3:containsOwn(Laufzeit des Vertrags, der Rahmenvereinbarung " +
                    "oder des dynamischen Beschaffungssystems) + dl", document);
        }
        if(datesElem == null){
            return null;
        }
        boolean first = true;
        String date = datesElem.selectFirst("dd").ownText();
        for(String part: date.split(" ")){
            if(part.matches(".*[0-9]*\\.[0-9]*\\.[0-9]*.*")){
                if(first){
                    first = false;
                } else {
                    return part.replaceAll("[^\\.[0-9]]", "");
                }
            }
        }
        return null;
    }

    /**
     * Parses lots.
     * @param document document to be parsed
     * @return list of parsed lots
     */
    private static List<ParsedTenderLot> parseLots(final Element document){
        List<ParsedTenderLot> parsedLots = new ArrayList<>();
        Element lotsTable = JsoupUtils.selectFirst("table#resultList", document);
        if(lotsTable == null){
            return null;
        }

        for(Element lotElem: lotsTable.select("tr")){
            lotElem = lotElem.selectFirst("td");
            if(lotElem != null && !lotElem.ownText().isEmpty()){
                String[] lotInfo = lotElem.toString().split("<br>");
                String lotNumber = lotInfo[0];
                String[] lotNumberSplit = lotNumber.split(">");
                if(lotNumberSplit.length > 1){
                    lotNumber = lotNumberSplit[1];
                    lotNumberSplit = lotNumber.split(" ");
                    if(lotNumberSplit.length > 1){
                        lotNumber = lotNumberSplit[lotNumberSplit.length - 1];
                    }
                }
                ParsedTenderLot lot = new ParsedTenderLot()
                        .setLotNumber(lotNumber).setDescription(lotInfo[1]);
                if(lotInfo.length > 2 && lotInfo[2].contains("CPV: ")){
                    lot.addCpv(new ParsedCPV().setCode(lotInfo[2].replace("CPV: ", ""))
                            .setIsMain(String.valueOf(true)));
                }
                parsedLots.add(lot);
            }
        }

        return parsedLots;
    }

    /**
     * Parses award criteria.
     * @param document document to be parsed
     * @return parsed award criteria
     */
    private static List<ParsedAwardCriterion> parseAwardCriteria(final Element document){
        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();
        Element criteriaElem = JsoupUtils.selectFirst("*:containsOwn(Zuschlagskriterien)", document);
        if(criteriaElem != null){
            if(criteriaElem.nextElementSibling() != null){
                for(Element crElem: criteriaElem.nextElementSibling().select("dd")){
                    List<String> criteria = Arrays.asList(crElem.toString()
                            .replace("\n", "").replace("<dd>", "").replace("</dd>", "")
                            .replace("&nbsp;", "").split("<br>"));
                    for(String criterion: criteria){
                        if(criterion.isEmpty() || criterion.startsWith("<")){
                            continue;
                        }
                        if(criterion.contains("Gewichtung")){
                            awardCriteria.add(new ParsedAwardCriterion()
                                    .setName(criterion.split(" Gewichtung")[0])
                                    .setWeight(criterion.split(" Gewichtung")[1]));
                        } else {
                            awardCriteria.add(new ParsedAwardCriterion()
                                    .setName(criterion));
                        }

                    }
                }
            }
        }
        if(awardCriteria.isEmpty()){
            return null;
        }
        return awardCriteria;
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
        Elements cpvs = document.select("b:containsOwn(CPV:)");

        if (cpvs == null || cpvs.isEmpty()) {
            return null;
        }
        cpvs = cpvs.get(0).parent().parent().parent().select("tr");
        final List<ParsedCPV> parsedCpvs = new ArrayList<>();

        for (Element cpv : cpvs) {
            String code = cpv.text();
            if(code != null && !code.isEmpty()){
                for(String part: code.split(" ")){
                    if(!part.isEmpty() && part.matches("[0-9]*")){
                        code = part;
                    }
                }
                parsedCpvs.add(new ParsedCPV()
                        .setCode(code)
                        .setIsMain(String.valueOf(parsedCpvs.isEmpty())));
            }
        }

        return parsedCpvs;
    }
}
