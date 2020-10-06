package eu.datlab.worker.lt.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;


/**
 * VPT handler for parsing procurement procedures reports.
 */
public final class CVPISTenderMWHandler {


    /**
     * Private constructor.
     */
    private CVPISTenderMWHandler() {

    }


    /**
     * Parses VPT form.
     *
     * @param document        document to be parsed
     * @param url             url
     * @param publicationDate date of publishing of the tender
     * @return parsed tender
     */
    public static ParsedTender parse(final Element document, final String url, final String publicationDate) {
        ParsedTender parsedTender = new ParsedTender();
        Element tmpElement;
        ParsedBody contractingAuthority = parseContractingAuthority(document);
        tmpElement = JsoupUtils.selectFirst("td:contains(PIRKIMO NUMERIS) ~ td", document);
        if (tmpElement != null && tmpElement.text() != null && !tmpElement.text().isEmpty()) {
            parsedTender.setBuyerAssignedId(tmpElement.text());
        }
        String buyerType;
        tmpElement = JsoupUtils.selectFirst("td:contains(PERKANČIOSIOS ORGANIZACIJOS TIPO KODAS) ~ td", document);
        if (tmpElement != null) {
            buyerType = tmpElement.text();
            if (!buyerType.isEmpty()) {
                contractingAuthority.setBuyerType(buyerType);
            }
        }

        parsedTender
                .addBuyer(contractingAuthority)
                .addFunding(new ParsedFunding()
                        .setIsEuFund(parseYesNoCheckbox("AR PIRKIMAS YRA SUSIJĘS SU PROJEKTU IR/ARBA PROGRAMA", document)))
                .setIsCentralProcurement(parseYesNoCheckbox("AR PIRKIMAS ATLIEKAMAS CENTRINĖS VIEŠŲJŲ PIRKIMŲ", document));


        String awardedToAnotherCA = parseYesNoCheckbox("AR PIRKIMO ĮGALIOJIMAI BUVO SUTEIKTI " +
                "KITAI PERKANČIAJAI ORGANIZACIJAI", document);
        parsedTender.setIsOnBehalfOf(awardedToAnotherCA);
        if (awardedToAnotherCA != null && awardedToAnotherCA.equals(String.valueOf(true))) {
            tmpElement = JsoupUtils.selectFirst("table:contains(Įgaliotosios organizacijos pavadinimas, kodas," +
                    " adresas ir tipas) ~ table", document);
            if (tmpElement != null) {
                parsedTender.addOnBehalfOf(parseOnBehalfOf(tmpElement));
            }
        }

        String supplyType = parseCheckbox("PREKIŲ PIRKIMO TIPAS", document);
        if (supplyType == null) {
            supplyType = parseClThinborderallField("PASLAUGŲ KATEGORIJA", document);
            if(supplyType != null && !supplyType.isEmpty()){
                supplyType = "SERVICES";
            }
        }
        if (supplyType == null || supplyType.isEmpty()) {
            supplyType = parseCheckbox("DARBŲ TIPAS", document);
        }

        parsedTender
                .setIsOnBehalfOf(awardedToAnotherCA)
                .setSupplyType(supplyType)
                .setTitle(parseClThinborderallField("PIRKIMO OBJEKTO PAVADINIMAS", document))
                .setCpvs(parseCpvs(document))
                .setLots(parseLots(document, parseBidders(document)))
                .setNationalProcedureType(parseClThinborderallField("PIRKIMO BŪDO PAVADINIMAS", document))
                .setIsDps(parseYesNoCheckbox("AR PIRKIMAS ATLIEKAMAS TAIKANT DINAMINĘ PIRKIMŲ SISTEMĄ", document))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(PublicationSources.LT_CVPIS)
                        .setHumanReadableUrl(url)
                        .setSourceFormType(parseSourceFormType(document))
                        .setDispatchDate(parseClThinborderallField("Šio skelbimo išsiuntimo dat", document))
                        .setPublicationDate(publicationDate)
                        .setSourceTenderId(parsedTender.getBuyerAssignedId()))
                .addPublications(parsePublications(document));


        String selectionMethod = parseSelectionMethod(document);
        parsedTender.setSelectionMethod(selectionMethod);
        // if selection method is different for some lots
        if (selectionMethod != null && selectionMethod.contains("Skirtingoms pirkimo objekto")) {
            Element tableHeader = JsoupUtils.selectFirst("tr:contains(Vertinimo kriterijus)", document);
            if (tableHeader != null && tableHeader.nextElementSibling() != null && tableHeader.nextElementSibling()
                    .nextElementSibling() != null) {
                String lotNumbersByMEAT = tableHeader.nextElementSibling().select("td").get(1).text();
                String lotNumbersByLowestPrice = tableHeader.nextElementSibling().nextElementSibling().select("td").get(1).text();
                for (String lotNumberString : lotNumbersByMEAT.split(" ")) {
                    int lotNumber = Integer.parseInt(lotNumberString.replace(",", ""));
                    for (ParsedTenderLot lot : parsedTender.getLots()) {
                        if (lot.getLotNumber().equals(String.valueOf(lotNumber))) {
                            lot.setSelectionMethod("Ekonomiškai naudingiausio pasiūlymo");
                            break;
                        }
                    }
                }
                for (String lotNumberString : lotNumbersByLowestPrice.split(" ")) {
                    int lotNumber = Integer.parseInt(lotNumberString.replace(",", ""));
                    for (ParsedTenderLot lot : parsedTender.getLots()) {
                        if (lot.getLotNumber().equals(String.valueOf(lotNumber))) {
                            lot.setSelectionMethod("Mažiausios kainos");
                            break;
                        }
                    }
                }
            }
        }
        Element endOfProcedures = JsoupUtils.selectFirst("table:contains(Procedūrų pabaiga) ~ table", document);
        if (endOfProcedures != null) {
            Elements rows = endOfProcedures.select("tr");
            if (rows != null) {
                String lotNumber = rows.get(1).select("td").get(1).text();
                if (lotNumber != null && !lotNumber.isEmpty()) {
                    for (ParsedTenderLot lot : parsedTender.getLots()) {
                        if (lot.getLotNumber().equals(lotNumber)) {
                            lot.setIsAwarded(String.valueOf(true)).setAwardDecisionDate(rows.get(1).select("td").get(2).text());
                        }
                    }
                }
            }
        }


        Element resultsTable = JsoupUtils.selectFirst("table:contains(Procedūrų pabaiga) ~ table", document);
        if (resultsTable != null) {
            parseAdditionalInfoAboutResults(document, resultsTable, parsedTender);
        }

        // At-8 form
        if (JsoupUtils.selectFirst("*:contains(SUTARTIES (PRELIMINARIOSIOS SUTARTIES) PABAIGA), " +
                "*:contains(PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) ĮVYKDYMO REZULTATAI)", document) != null
                && parsedTender.getLots() != null && parsedTender.getLots().size() == 1) {
            parseSpecialSectionsForAt8(document, parsedTender, publicationDate);
        }

        return parsedTender;
    }

    /**
     * Parses address from raw address.
     *
     * @param rawAddress raw address to be parsed
     * @return paarsed address
     */
    private static ParsedAddress parseAddressFromRawAddress(final String rawAddress) {
        if (rawAddress == null || rawAddress.isEmpty()) {
            return null;
        }
        String postcode = null, city = null;
        if (rawAddress.split(", ").length > 1) {
            postcode = rawAddress.split(", ")[1];
            if (postcode.split(" ").length > 1) {
                city = postcode.split(" ")[1];
                postcode = postcode.split(" ")[0];
            }
        }
        return new ParsedAddress()
                .setRawAddress(rawAddress)
                .setStreet(rawAddress.split(",")[0])
                .setPostcode(postcode)
                .setCity(city);
    }

    /**
     * Parses onBehalfOf certification authority.
     *
     * @param document document to be parsed
     * @return parsed certification authority
     */
    private static ParsedBody parseOnBehalfOf(final Element document) {
        ParsedBody anotherCA = new ParsedBody()
                .setName(parseClThinborderallField("Pavadinimas", document))
                .addBodyId(new BodyIdentifier()
                        .setId(parseClThinborderallField("Kodas", document))
                        .setScope(BodyIdentifier.Scope.LT)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                .setBuyerType(parseClThinborderallField("Organizacijos tipo kodas", document));
        String addressAndPhone = parseClThinborderallField("Adresas, telefonas", document);
        if (addressAndPhone != null) {
            String rawAddress = addressAndPhone.split("\\+")[0].split("Tel")[0].split("tel")[0];
            String phone = null;
            if (addressAndPhone.contains("+")) {
                phone = "+" + addressAndPhone.split("\\+")[1];
            } else if (addressAndPhone.contains("el.")) {
                phone = addressAndPhone.split("el.")[1];
            }
            anotherCA.setPhone(phone).setAddress(parseAddressFromRawAddress(rawAddress));
        }
        return anotherCA;
    }

    /**
     * Parses additional information about contract results.
     *
     * @param resultsTable table with results
     * @param parsedTender parsed tender to be updated
     * @param document     document to be parsed
     */
    private static void parseAdditionalInfoAboutResults(final Element document, final Element resultsTable,
                                                        final ParsedTender parsedTender) {
        Elements rows = resultsTable.select("tr");
        if (rows != null && !rows.isEmpty() && rows.get(0).select("td").size() > 2) {
            for (int i = 1; i < rows.size(); i++) {
                String lotNumbers = rows.get(i).select("td").get(1).text();
                if (lotNumbers.isEmpty()) {
                    continue;
                }
                String[] dates = rows.get(i).select("td").get(2).text().split(" |,|, ");
                int dateIndex = 0;
                if (lotNumbers.split(" |,|, ").length == dates.length) {
                    dateIndex = 1;
                }
                for (String lotNumber : lotNumbers.split(" |,|, ")) {
                    if (i == 1) {
                        ParsedTenderLot lotByNumber = getLotByNumber(lotNumber, parsedTender.getLots());
                        if (lotByNumber != null) {
                            lotByNumber.setStatus("AWARDED").setAwardDecisionDate(dates[dateIndex == 0 ? 0 : dateIndex - 1]);
                            if (dateIndex != 0) {
                                dateIndex++;
                            }
                        }
                    } else {
                        ParsedTenderLot lotByNumber = getLotByNumber(lotNumber, parsedTender.getLots());
                        if (lotByNumber != null) {
                            lotByNumber.setStatus("CANCELLED").setCancellationDate(dates[dateIndex == 0 ? 0 : dateIndex - 1])
                                    .setCancellationReason(rows.get(i).select("td").get(0).text());
                            if (dateIndex != 0) {
                                dateIndex++;
                            }
                        }
                    }
                }
            }
        }

        Element infoAboutWinner = JsoupUtils
                .selectFirst("table:contains(Informacija apie pirkimo sutartį (preliminariąją sutartį)) ~ table", document);
        if (infoAboutWinner != null && parseClThinborderallField("Pirkimo objekto dalies (-ių) numeris", infoAboutWinner) != null) {
            ParsedTenderLot lotByNumber = getLotByNumber(parseClThinborderallField("Pirkimo objekto dalies (-ių) numeris", infoAboutWinner),
                    parsedTender.getLots());
            infoAboutWinner = infoAboutWinner.nextElementSibling();
            if (lotByNumber != null && infoAboutWinner != null) {
                String winnersName = parseClThinborderallField("Tiekėjo pavadinima", infoAboutWinner);
                if (winnersName != null) {
                    ParsedBid bid;
                    if (lotByNumber.getBids() == null || lotByNumber.getBids().size() == 0) {
                        lotByNumber.addBid(new ParsedBid());
                        bid = lotByNumber.getBids().get(0);
                    } else {
                        bid = getBidByBiddersName(winnersName, lotByNumber.getBids());
                        if (bid == null) {
                            lotByNumber.addBid(new ParsedBid());
                            bid = lotByNumber.getBids().get(0);
                        }
                    }
                    if (bid.getBidders() == null) {
                        BodyIdentifier bodyId = null;
                        if (winnersName.contains("(")) {
                            bodyId = new BodyIdentifier().setId(winnersName.split("\\(")[1].split("\\)")[0])
                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID).setScope(BodyIdentifier.Scope.LT);
                        }
                        bid.addBidder(new ParsedBody().setName(winnersName.split("\\(")[0]).addBodyId(bodyId));
                    }
                    bid.setIsWinning(String.valueOf(true));
                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    lotByNumber.setContractSignatureDate(parseClThinborderallField("Sutarties sudarymo data", infoAboutWinner));

                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    lotByNumber.setContractSignatureDate(parseClThinborderallField("Sutarties sudarymo data", infoAboutWinner));

                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    ParsedPrice price = new ParsedPrice().setNetAmount(parseClThinborderallField("apimtis", infoAboutWinner))
                            .setCurrency("LTL");
                    if (price.getNetAmount() != null) {
                        bid.setPrice(price);
                    }

                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    lotByNumber.setEstimatedCompletionDate(parseClThinborderallField("Numatoma sutarties įvykdymo data", infoAboutWinner));
                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    bid.setIsSubcontracted(parseCheckbox("Ar ketinama sudaryti subrangos ar subtiekimo sutartį", infoAboutWinner));
                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    if (bid.getIsSubcontracted() != null && bid.getIsSubcontracted().equals("TAIP")) {
                        String netAmount = parseClThinborderallField("Apimtis", infoAboutWinner);
                        if (netAmount != null) {
                            bid.setSubcontractedValue(new ParsedPrice().setNetAmount(netAmount).setCurrency("LTL"));
                        }
                        bid.setSubcontractedProportion(parseClThinborderallField("Dalis procentais", document));
                    }
                    infoAboutWinner = infoAboutWinner.nextElementSibling();
                    if(infoAboutWinner == null){
                        return;
                    }
                    parsedTender.setIsCentralProcurement(parseYesNoCheckbox("buvo atliktas centralizuotai", infoAboutWinner));
                }
            }
        }
    }

    /**
     * Finds bid by the name of its bidder.
     *
     * @param biddersName bidders name to be find
     * @param bids        list of bids
     * @return bid of the bidder with the given name
     */
    private static ParsedBid getBidByBiddersName(final String biddersName, final List<ParsedBid> bids) {
        for (ParsedBid oneBid : bids) {
            if(oneBid.getBidders() != null){
                if (oneBid.getBidders().get(0).getName().split("\\(")[0].replace(" ", "")
                        .equals(biddersName.split("\\(")[0].replace(" ", ""))) {
                    return oneBid;
                }
            }
        }
        return null;
    }

    /**
     * Parses special sections for the form At-8.
     *
     * @param document        document to be parsed
     * @param parsedTender    parsedTender to be updated
     * @param publicationDate publication date
     */
    private static void parseSpecialSectionsForAt8(final Element document, final ParsedTender parsedTender,
                                                   final String publicationDate) {
        ParsedTenderLot firstLot = parsedTender.getLots().get(0);
        firstLot.setStatus(parseLotStatus(document));
        ParsedPayment payment = null;
        Element paymentInfo = JsoupUtils.selectFirst("*:containsOwn(FAKTINĖ (ĮVYKDYTOS AR NUTRAUKTOS) SUTARTIES " +
                "(PRELIMINARIOSIOS SUTARTIES) APIMTIS), *:containsOwn(FAKTINĖ SUTARTIES (PRELIMINARIOSIOSSUTARTIES) APIMTIS)," +
                "*:containsOwn(FAKTINĖ PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) VERTĖ)", document);
        if (paymentInfo != null) {
            String paymentPrice = parseClThinborderallField("FAKTINĖ (ĮVYKDYTOS AR NUTRAUKTOS) SUTARTIES " +
                    "(PRELIMINARIOSIOS SUTARTIES) APIMTIS, FAKTINĖ SUTARTIES (PRELIMINARIOSIOSSUTARTIES) APIMTIS, " +
                    "FAKTINĖ PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) VERTĖ", document);
            String currency = paymentInfo.text().contains("EUR") ? "EUR" : "LTL";
            String dispatchDate = parsedTender.getPublications().get(0).getDispatchDate();
            String paymentYear = dispatchDate == null ? publicationDate : dispatchDate;
            payment = new ParsedPayment()
                    .setPrice(new ParsedPrice().setNetAmount(paymentPrice).setCurrency(currency))
                    .setPaymentDate(paymentYear);
        }
        String lotNumber = parseClThinborderallField("PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) OBJEKTAS", document);
        if (lotNumber != null && !lotNumber.isEmpty()) {
            firstLot.setLotNumber(lotNumber);
        }

        if (firstLot.getBids() == null || firstLot.getBids().size() == 0) {
            Element supplierInfo = JsoupUtils.selectFirst("table:contains(TIEKĖJAS) ~ table", document);
            if (supplierInfo != null) {
                firstLot.addBid(new ParsedBid()
                        .setIsWinning(String.valueOf(true))
                        .addBidder(new ParsedBody()
                                .setName(parseClThinborderallField("Pavadinimas", supplierInfo))
                                .addBodyId(new BodyIdentifier()
                                        .setId(parseClThinborderallField("Kodas", supplierInfo))
                                        .setScope(BodyIdentifier.Scope.LT).setType(BodyIdentifier.Type.ORGANIZATION_ID)))
                        .addPayment(payment));
                if (firstLot.getBids().get(0).getBidders().get(0).getName() == null) {
                    supplierInfo = JsoupUtils.selectFirst("table:contains(TIEKĖJAS)", document);
                    if (supplierInfo != null) {
                        Elements tds = supplierInfo.child(0).children();
                        if (tds.size() > 1) {
                            tds = tds.get(1).children();
                            if (tds.size() > 2) {
                                String id = tds.get(1).selectFirst("td.clThinborderall").text();
                                String name = tds.get(2).selectFirst("td.clThinborderall").text();
                                if (id != null && !id.isEmpty()) {
                                    firstLot.getBids().get(0).getBidders().get(0).setName(name).getBodyIds().get(0).setId(id);
                                }
                            }
                        }
                    } else {
                        firstLot.getBids().get(0).setBidders(null);
                    }
                }
            }
        }
    }

    /**
     * Finds lot with given lot number.
     *
     * @param lotNumber lot number to find
     * @param lots      list of tender lots
     * @return lot with given lot number
     */
    private static ParsedTenderLot getLotByNumber(final String lotNumber, final List<ParsedTenderLot> lots) {
        for (ParsedTenderLot lot : lots) {
            if (lot.getLotNumber().equals(lotNumber)) {
                return lot;
            }
        }
        return null;
    }

    /**
     * Parses lot status.
     *
     * @param document document to be parsed
     * @return parsed lot status
     */
    private static String parseLotStatus(final Element document) {
        Element row = JsoupUtils.selectFirst("tr:contains(Sutartis (preliminarioji sutartis) įvykdyta), " +
                "tr:contains(Sutartis (preliminarioji sutartis) įvykdyta visiškai)", document);
        if (row != null) {
            if (row.selectFirst("input") != null && row.selectFirst("input").hasAttr("checked")) {
                return "FINISHED";
            }
            row = JsoupUtils.selectFirst("tr:contains(Sutartis (preliminarioji sutartis) nutraukta), " +
                    "tr:contains(Sutartis (preliminarioji sutartis) įvykdyta ne visiškai)", document);
            if (row != null) {
                if (row.selectFirst("input") != null && row.selectFirst("input").hasAttr("checked")) {
                    return "CANCELLED";
                }
            }
            row = JsoupUtils.selectFirst("tr:contains(Pirkimo sutartis pripažinta negaliojančia), " +
                    "tr:contains(Sutartis (preliminarioji sutartis) nutraukta)", document);
            if (row != null) {
                if (row.selectFirst("input") != null && row.selectFirst("input").hasAttr("checked")) {
                    return "CANCELLED";
                }
            }
            row = JsoupUtils.selectFirst("tr:contains(Kiti pagrindai)", document);
            if (row != null) {
                if (row.selectFirst("input") != null && row.selectFirst("input").hasAttr("checked")) {
                    return "CANCELLED";
                }
            }
        }
        return null;
    }

    /**
     * Parses form type if it is present in document.
     *
     * @param document document to be parsed
     * @return parsed form type or null if document does not contain information about for type
     */
    private static String parseSourceFormType(final Element document) {
        Element elementWithForm = document.selectFirst("table").selectFirst("*:containsOwn(form)");
        if (elementWithForm != null) {
            for (String part : elementWithForm.ownText().split(" ")) {
                if (part.contains("-")) {
                    return part;
                }
            }
        }
        return null;
    }

    /**
     * Parses cpvs.
     *
     * @param document document to be parsed
     * @return list of parsed cpvs
     */
    private static List<ParsedCPV> parseCpvs(final Element document) {
        List<ParsedCPV> cpvs = new ArrayList<>();
        String cpv = parseClThinborderallField("PAGRINDINIS PIRKIMO OBJEKTO KODAS PAGAL BVPŽ", document);
        if (cpv != null && !cpv.isEmpty()) {
            cpvs.add(new ParsedCPV()
                    .setCode(cpv)
                    .setIsMain(String.valueOf(true)));
        }
        cpv = parseClThinborderallField("PAPILDOMI PIRKIMO OBJEKTO KODAI PAGAL BVPŽ", document);
        if (cpv != null && !cpv.isEmpty()) {
            for (String cpvCode : cpv.split(" ")) {
                cpvCode = cpvCode.replace(",", "");
                cpvs.add(new ParsedCPV()
                        .setCode(cpvCode)
                        .setIsMain(String.valueOf(false)));
            }
        }
        if (cpvs.isEmpty()) {
            cpv = parseClThinborderallField("PIRKIMO OBJEKTO KODAS, BVPŽ", document);
            if (cpv != null && !cpv.isEmpty()) {
                String[] cpvCodes = cpv.split(" ");
                cpvs.add(new ParsedCPV()
                        .setIsMain(String.valueOf(true))
                        .setCode(cpvCodes[0].replace(",", "")));
                for (int i = 1; i < cpvCodes.length; i++) {
                    cpvs.add(new ParsedCPV()
                            .setIsMain(String.valueOf(false))
                            .setCode(cpvCodes[i].replace(",", "")));
                }
            }
        }
        return cpvs;
    }


    /**
     * Parses selection method.
     *
     * @param document document to be parsed
     * @return parsed selection method
     */
    private static String parseSelectionMethod(final Element document) {
        Element table = JsoupUtils.selectFirst("table:contains(PASIŪLYMŲ VERTINIMO KRITERIJAI)", document); // needs test
        if (table == null) {
            return null;
        }
        Elements rows = table.select("tr");
        for (int i = 1; i < 4; i++) {
            if (rows.get(i).selectFirst("input").hasAttr("checked")) {
                String selectionMethod = rows.get(i).selectFirst("td").text();
                if (selectionMethod.isEmpty()) {
                    return null;
                }
                return selectionMethod;
            }
        }
        return null;
    }

    /**
     * Parses bidders.
     *
     * @param document document to be parsed
     * @return list of parsed bidders
     */
    private static List<ParsedBody> parseBidders(final Element document) {
        Element table = JsoupUtils.selectFirst("table:contains(DALYVIAI) ~ table", document);
        if (table == null) {
            return null;
        }
        Elements rows = table.select("tr");
        if (rows == null) {
            return null;
        }
        List<ParsedBody> bidders = new ArrayList<>();
        for (Element row : rows) {
            if (row.selectFirst("td").text().equals("Pavadinimas") || row.select("td").get(0).text().isEmpty()) {
                continue;
            }
            ParsedBody bidder = new ParsedBody()
                    .setName(row.select("td").get(0).text())
                    .addBodyId(new BodyIdentifier()
                            .setId(row.select("td").get(1).text())
                            .setScope(BodyIdentifier.Scope.LT)
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                    .setAddress(new ParsedAddress().setRawAddress(row.select("td").get(2).text())
                            .setCountry(row.select("td").get(3).text()));
            bidders.add(bidder);
        }
        return bidders;
    }

    /**
     * Parses previous publications.
     *
     * @param document document to be parsed
     * @return list of parsed publications
     */
    private static List<ParsedPublication> parsePublications(final Element document) {
        List<ParsedPublication> publications = new ArrayList<>();
        ParsedPublication previousPublicationPriorNotice = parsePublication("1. IŠANKSTINIAI SKELBIMAI APIE PIRKIMĄ", document);
        if (previousPublicationPriorNotice != null) {
            publications.add(previousPublicationPriorNotice);
        }
        ParsedPublication previousPublicationContractNotice = parsePublication("2. SKELBIMAI APIE PIRKIMĄ", document);
        if (previousPublicationContractNotice != null) {
            publications.add(previousPublicationContractNotice);
        }
        ParsedPublication previousPublicationSimplifiedContract =
                parsePublication("3. SUPAPRASTINTI SKELBIMAI APIE PIRKIMĄ DINAMINĖJE PIRKIMŲ SISTEMOJE", document);
        if (previousPublicationSimplifiedContract != null) {
            publications.add(previousPublicationSimplifiedContract);
        }
        ParsedPublication previousPublicationNoticeOnPurchaseWithSimplifiedProcedure =
                parsePublication("4. SUPAPRASTINTŲ PIRKIMŲ ATVEJU, " +
                        "JEIGU BUVO SKELBTAS INFORMACINIS PRANEŠIMAS APIE SPRENDIMĄ PIRKTI PREKES", document);
        if (previousPublicationNoticeOnPurchaseWithSimplifiedProcedure != null) {
            publications.add(previousPublicationNoticeOnPurchaseWithSimplifiedProcedure);
        }
        return publications;
    }

    /**
     * Parses publications.
     *
     * @param selector the name of the section to be parsed
     * @param document document to be parsed
     * @return parsed publications
     */
    private static ParsedPublication parsePublication(final String selector, final Element document) {
        Element data = JsoupUtils.selectFirst("table:contains(" + selector + ") ~ table", document);
        if (data == null || data.child(0) == null) {
            return null;
        }
        Elements rows = data.child(0).children();
        if (rows == null || rows.size() < 2 || rows.size() > 3) {
            return null;
        }
        ParsedPublication publication = new ParsedPublication();
        if (rows.size() == 3) {
            publication.setDispatchDate(rows.get(0).select("td.clThinborderall").text());
            if (rows.get(1).select("td:containsOwn(skelbimo paskelbimo Centrinėje viešųjų pirkimų " +
                    "informacinėje sistemoje data)") == null) {
                publication.setSourceId(rows.get(1).select("td.clThinborderall").text());
            }
            publication.setPublicationDate(rows.get(2).select("td.clThinborderall").text());
        } else {
            publication.setSourceId(rows.get(0).select("td.clThinborderall").text());
            publication.setPublicationDate(rows.get(1).select("td.clThinborderall").text());
        }
        if ((publication.getDispatchDate() == null || publication.getDispatchDate().isEmpty())
                && (publication.getPublicationDate() == null || publication.getPublicationDate().isEmpty())
                && (publication.getSourceId() == null || publication.getSourceId().isEmpty())) {
            return null;
        }
        return publication;
    }

    /**
     * Parses lots.
     *
     * @param document document to be parsed
     * @param bidders  list of bidders
     * @return list of parsed lots
     */
    private static List<ParsedTenderLot> parseLots(final Element document, final List<ParsedBody> bidders) {
        List<ParsedTenderLot> parsedLots = new ArrayList<>();
        Element tableWithLots = JsoupUtils.selectFirst("table:contains(PIRKIMO OBJEKTO DALYS)", document);
        if (tableWithLots == null) {
            parsedLots.add(new ParsedTenderLot().setLotNumber(String.valueOf(1)));
            return parsedLots;
        }
        tableWithLots = tableWithLots.nextElementSibling();
        if (tableWithLots == null) {
            parsedLots.add(new ParsedTenderLot().setLotNumber(String.valueOf(1)));
            return parsedLots;
        }
        tableWithLots = tableWithLots.nextElementSibling();
        if (tableWithLots == null) {
            parsedLots.add(new ParsedTenderLot().setLotNumber(String.valueOf(1)));
            return parsedLots;
        }
        Elements rowsWithLots = tableWithLots.select("tr"); // not sure if different lots are in different tr or tables
        int nextLotNumber = 1;
        if (rowsWithLots == null || rowsWithLots.isEmpty()) {
            parsedLots.add(new ParsedTenderLot().setLotNumber("1"));
        } else {
            for (Element rowWithLotInfo : rowsWithLots) {
                Elements columns = rowWithLotInfo.select("td");
                if (columns.size() != 3) {
                    parsedLots.add(new ParsedTenderLot().setLotNumber(String.valueOf(1)));
                    return parsedLots;
                }
                ParsedTenderLot lot = new ParsedTenderLot();
                lot.setLotNumber(columns.get(0).text())
                        .setTitle(columns.get(1).text());
                if (lot.getLotNumber().isEmpty()) {
                    lot.setLotNumber(String.valueOf(nextLotNumber));
                    nextLotNumber++;
                }
                if (lot.getTitle().isEmpty()) {
                    lot.setTitle(null);
                }
                if (!columns.get(2).text().isEmpty()) {
                    lot.addCpv(new ParsedCPV()
                            .setCode(columns.get(2).text())
                            .setIsMain(String.valueOf(true)));
                }
                parsedLots.add(lot);
            }
        }

        if (parsedLots.isEmpty()) {
            parsedLots.add(new ParsedTenderLot().setLotNumber(String.valueOf(1)));
        }


        Element bidsTable = JsoupUtils.selectFirst("table:contains(pasiūlymų eilė arba " +
                "priimtas sprendimas) ~ table", document);
        if (bidsTable != null) {
            Elements bidsRows = bidsTable.select("tr");
            for (int i = 1; i < bidsRows.size(); i++) {
                ParsedTenderLot selectedLot = null;
                String lotNumber = bidsRows.get(i).select("td").get(0).text();
                for (ParsedTenderLot lot : parsedLots) {
                    if (lot.getLotNumber().equals(lotNumber)) {
                        selectedLot = lot;
                        break;
                    }
                }
                if (selectedLot != null) {
                    selectedLot.addBid(new ParsedBid()
                            .setIsWinning(String.valueOf(false))
                            .setBidId(bidsRows.get(i).select("td").get(1).text())
                            .addBidder(findBidderByName(bidsRows.get(i).select("td").get(2).text(), bidders))
                            .setPrice(new ParsedPrice()
                                    .setNetAmount(bidsRows.get(i).select("td").get(4).text())
                                    .setCurrency(bidsRows.get(i).select("td").get(5).text())));
                }
            }
        }

        if (parsedLots.size() == 1) {
            // we delete every winner from the list while parsing winning bids,
            // so now the list contains only bidders whose bid is not winning
            if(bidders != null) {
                for (ParsedBody bidder : bidders) {
                    parsedLots.get(0).addBid(new ParsedBid().setIsWinning(String.valueOf(false)).addBidder(bidder));
                }
            }
        }
        return parsedLots;
    }

    /**
     * Finds bidder by his name.
     *
     * @param name    name of bidder to find
     * @param bidders list of bidders
     * @return bidder with given name
     */
    private static ParsedBody findBidderByName(final String name, final List<ParsedBody> bidders) {
        if(bidders == null || bidders.isEmpty()){
            return null;
        }
        for (ParsedBody bidder : bidders) {
            if (bidder.getName().equals(name)) {
                bidders.remove(bidder);
                return bidder;
            }
        }
        return null;
    }

    /**
     * Parses checkbox.
     *
     * @param selector the name of the field
     * @param document document to be parsed
     * @return the value from checkbox or null if the is no checkbox with this name or something is wrong with its structure
     */
    private static String parseCheckbox(final String selector, final Element document) {
        Element parentElement = JsoupUtils.selectFirst("tr:contains(" + selector + ") ~ tr", document);
        if (parentElement == null) {
            return null;
        }
        Elements fields = parentElement.select("td");
        if (fields == null) {
            return null;
        }
        for (Element field : fields) {
            if (field.selectFirst("input").hasAttr("checked")) {
                return field.text();
            }
        }
        return null;
    }

    /**
     * Parses the value in the field with class clThinborderall.
     *
     * @param selector the name of field
     * @param document document to parse
     * @return value from the clThinborderall field
     */
    private static String parseClThinborderallField(final String selector, final Element document) {
        Element field = null;
        for (String partOfSelector : selector.split(", ")) {
            field = JsoupUtils.selectFirst("td:contains(" + partOfSelector + ") ~ td", document);
            if (field != null) {
                break;
            }
        }
        if (field == null) {
            return null;
        }
        field = JsoupUtils.selectFirst("td.clThinborderall", field);
        if (field == null) {
            return null;
        }
        if (field.ownText().isEmpty()) {
            return null;
        }
        return field.ownText().replace("&nbsp", "");
    }


    /**
     * Parses the value of checkbox (yes or no).
     *
     * @param selector the name of field
     * @param document document to parse
     * @return the value in checkbox
     */
    private static String parseYesNoCheckbox(final String selector, final Element document) {
        Element tmpElement = JsoupUtils.selectFirst("td:contains(" + selector + ") ~ td", document);
        if (tmpElement != null) {
            tmpElement = JsoupUtils.selectFirst("input", tmpElement);
            if (tmpElement != null) {
                return String.valueOf(tmpElement.hasAttr("checked"));
            }
        }
        return null;
    }


    /**
     * Parses contracting authority.
     *
     * @param document document to be parsed
     * @return parses body (contracting authority)
     */
    private static ParsedBody parseContractingAuthority(final Element document) {
        ParsedBody contractingAuthority = new ParsedBody();
        Elements underlinedTexts = JsoupUtils.select("u", document);
        if (underlinedTexts != null && underlinedTexts.size() > 1) {
            String[] splitData = underlinedTexts.get(1).text().split(", ");
            String rawAddress = splitData[2];
            if(splitData.length > 3){
                rawAddress += ", " + splitData[3];
                if(splitData.length > 4){
                    rawAddress += splitData[4].split(" ")[0];
                }
            }
            contractingAuthority
                    .setName(underlinedTexts.get(0).text())
                    .addBodyId(new BodyIdentifier()
                            .setId(splitData[1])
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                            .setScope(BodyIdentifier.Scope.LT))
                    .setAddress(new ParsedAddress().setRawAddress(rawAddress));
            for (String part : splitData) {
                if (part.contains("@")) {
                    contractingAuthority.setEmail(part);
                }
                if (part.contains("Tel.")) {
                    contractingAuthority.setPhone(part.contains(":")
                            ? part.split(":")[1].replace(" ", "") : part);
                }
            }
        }
        return contractingAuthority;
    }

}
