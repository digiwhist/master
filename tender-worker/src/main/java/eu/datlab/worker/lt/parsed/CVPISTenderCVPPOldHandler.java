package eu.datlab.worker.lt.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;

import static eu.datlab.worker.lt.parsed.CVPISTenderParser.parseOwnText;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by michalriha on 05/06/2017.
 */
public final class CVPISTenderCVPPOldHandler {

    /**
     * Private constructor.
     */
    private CVPISTenderCVPPOldHandler() {
    }

    /**
     * Parse old form.
     *
     * @param document        document to be parsed
     * @param url             url
     * @param publicationDate date of publishing of the tender
     * @return parsed tender
     */
    public static ParsedTender parse(final Element document, final String url, final String publicationDate) {
        ParsedTender parsedTender = new ParsedTender()
                .setBuyerAssignedId(parseBuyerAssignedId(document))
                .addBuyer(new ParsedBody()
                        .setName(parseName(document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseBodyId(document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.LT)
                        )
                        .setPhone(parsePhone(document))
                        .setAddress(parseAddress(document))
                        .setEmail(parseIinTD("El.paštas", document))
                        .setContactPoint(parseIinTD("Kam", document))
                        .setBuyerType(parseIinTD(new String[]{
                                "Perkančiosios organizacijos tipa",
                                "veiklos sritis ar sritys:"
                        }, document))
                        .addMainActivity(parseIinTD(new String[]{
                                "Pagrindinė perkančiojo subjekto veiklos sritis ar s",
                                "veiklos sritis ar sritys:"
                        }, document)))
                .setIsOnBehalfOf(parseIinTD("ioji organizacija (institucija) perka kit, " +
                        "Perkančioji organizacija yra įgaliota kitos perkančiosios organizacijos atlikti pirkimą", document))
                .setTitle(parseTitle(document))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(parseIinTD(new String[]{
                                "Pagrindinė pristatymo vieta",
                                "atlikimo, prekių pristatymo ar paslaugų teikimo",
                                "agrindin� teikimo viet"
                        }, document))
                        .addNuts(parseIinTD("NUTS", document))
                )
                .setDescription(parseIinTD(new String[]{
                        "Sutarties ar pirkimo (-ų) aprašymas:",
                        "pas konkrečios sutarties aprašymas aprašym",
                        "rumpas sutarties ar pirkim"
                }, document))
                .setCpvs(parseCpvs(document))
                .setIsCoveredByGpa(parseIinTD("(GPA)", document))
                .setHasLots(parseIinTD("Pirkimo dalijimas", document))
                .setAreVariantsAccepted(parseIinTD("ma pateikti alternatyvius pa", document))
                .setEstimatedDurationInMonths(parseIinTD("ties trukm� arba �vykdymo termi", document))
                .setDeposits(parseIinTD(new String[]{
                        "os sąlygos.Sutarties įvykdymo užti",
                        "statai ir garantijos:"
                }, document))
                .setPersonalRequirements(parseIinTD("Informacija apie asmenin", document))
                .setEconomicRequirements(parseIinTD("Informacija apie asmenin", document))
                .setTechnicalRequirements(parseIinTD("Informacija apie asmenin", document))
                .setNationalProcedureType(parseIinTD(new String[]{
                        "rocedūros rūšis",
                        "Proced�ros r��is"
                }, document))
                .setSelectionMethod(parseIinTD("Sutarties sudarymo kriterijai", document))
                .setIsElectronicAuction(parseIinTD("elektroninis aukcion", document))
                .setDocumentsPayable(parseIinTD("mamas mokest", document))
                .setBidDeadline(parseIinTD("si�lym� ar pra�ym� leisti dalyvauti pri�mimo terminas", document))
                .addEligibleBidLanguage(parseIinTD("ai leisti dalyvauti", document))
                .setAwardDeadlineDuration(parseIinTD("rivalo u�tikrinti pasi�lymo galiojim� (atvira proced�ra)",
                        document))
                .setIsFrameworkAgreement(parseIinTD("r bus sudaroma preliminarioji sutar", document))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(parseIinTD("nansuojama Bendrij", document))
                )
                .setSupplyType(parseSupplyType(document))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(PublicationSources.LT_CVPIS)
                        .setHumanReadableUrl(url)
                        .setSourceFormType(parseFormType(document))
                        .setDispatchDate(parseIinTD(new String[]{
                                "Šio skelbimo išsiuntimo dat",
                                "o skelbimo i�siuntimo data"
                        }, document))
                        .setPublicationDate(publicationDate)
                        .setSourceTenderId(parseBuyerAssignedId(document)))
                .setFinalPrice(parseFinalPrice(document))
                .addLot(parseLot(document))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(false)
                        .setSource(PublicationSources.LT_CVPIS)
                        .setSourceFormType(JsoupUtils.selectText("i:containsOwn(Skelbimas apie pirkimą)", document))
                        .setSourceId(parseIinTD("Skelbimo OL numeri", document))
                        .setPublicationDate(parseIinTD("Skelbimo OL numeri", document)));
        Element estimatedDatesElem = JsoupUtils.selectFirst("td:contains(Sutarties trukmė arba įvykdymo terminas)", document);
        if (estimatedDatesElem != null) {
            String estimatedDates = estimatedDatesElem.ownText();
            if (!estimatedDates.isEmpty() && estimatedDates.contains("Pradžia") && estimatedDates.contains("Pabaiga")) {
                parsedTender.setEstimatedStartDate(estimatedDates.split("radžia")[1].split("abaiga")[0].replaceAll("[^0-9\\-]", ""));
                parsedTender.setEstimatedCompletionDate(estimatedDates.split("abaiga")[1].replaceAll("[^0-9\\-]", ""));
            }
        }

        return parsedTender;
    }

    /**
     * Parses form type.
     * @param document document to be parsed
     * @return parseed form type
     */
    private static String parseFormType(final Element document){
        String formType = JsoupUtils.selectText("*:containsOwn(skelbimas), *:containsOwn(SKELBIMAS)", document);
        if(formType == null){
            Element titleTable = JsoupUtils.selectFirst("table#Table1", document);
            if(titleTable != null){
                formType = titleTable.text();
            }
        }
        if(formType == null || formType.isEmpty()){
            return null;
        }
        return formType;
    }

    /**
     * Parses next tr.
     * @param selector selector
     * @param document document to be parsed
     * @return text in next tr
     */
    private static String parseNextLine(final String selector, final Element document) {
        String parsedText = null;
        for (String partOfSelector : selector.split(", ")) {
            Element nextTr = JsoupUtils.selectFirst("tr:contains(" + partOfSelector + ") ~ tr", document);
            if (nextTr != null) {
                if (nextTr.selectFirst("td") != null) {
                    nextTr = nextTr.selectFirst("td");
                }
                parsedText = nextTr.text();
                break;
            }
        }

        if (parsedText != null && !parsedText.isEmpty()) {
            return parsedText;
        }
        return null;
    }

    /**
     * Parses title.
     *
     * @param document document to be parsed
     * @return parsed title
     */
    private static String parseTitle(final Element document) {
        String[] selector = {
                "adinimas, kurį perkančioji organizacija (institucija) suteikė sutar",
                "ar perkantysisi subjektas sutei",
                "rkimo pavadinimas",
                "rganizacija (institucija) suteik� sutar�iai"};
        String title = parseIinTD(selector, document);
        if (title != null && !title.isEmpty()) {
            return title;
        }
        Element data = null;
        for (String partOfSelector : selector) {
            data = JsoupUtils.selectFirst("tr:contains(" + partOfSelector + ") ~ tr", document);
            if (data != null) {
                break;
            }
        }
        if (data == null) {
            return null;
        }
        title = data.text();
        if (title == null || title.isEmpty()) {
            return null;
        }
        return title;
    }

    /**
     * Parses address.
     *
     * @param document document to be parsed
     * @return parsed address
     */
    private static ParsedAddress parseAddress(final Element document) {
        if (document == null) {
            return null;
        }
        String rawAddress = parseIinTD("Adresas, telefonas, faks", document);
        if(rawAddress != null && !rawAddress.isEmpty()){
            rawAddress = rawAddress.split(", tel")[0];
        }

        ParsedAddress address = new ParsedAddress()
                .setRawAddress(rawAddress)
                .setUrl(parseIinTD(new String[]{
                        "ančiojo subjekto adresas",
                        "Interneto adresas"
                }, document));
        if(rawAddress != null && !rawAddress.isEmpty()){
            address.setStreet(rawAddress.split(",")[0]);
        }
        return address;
    }

    /**
     * Parses phone number.
     *
     * @param document document to be parsed
     * @return parsed phone number
     */
    private static String parsePhone(final Element document) {
        String phoneNumber = parseIinTD("Adresas, telefonas, faks", document);
        if (phoneNumber != null) {
            if (phoneNumber.contains("tel")) {
                phoneNumber = phoneNumber.split("tel")[1].split(",")[0]
                        .replace(".", "")
                        .replace(" ", "");
                return phoneNumber;
            }
        }
        return null;
    }

    /**
     * Parses buyer name.
     * @param document document to be parsed
     * @return parsed buyer name
     */
    private static String parseName(final Element document){
        String name = parseIinTD("IOJI ORGANIZACIJA, PERKANTYSIS SUBJEKTAS", document);
        if(name != null){
            name = name.split(" \\(")[0];
        }
        return name;
    }

    /**
     * Parses body Id.
     *
     * @param document document to be parsed
     * @return parsed body id
     */
    private static String parseBodyId(final Element document) {
        String id = parseIinTD("IOJI ORGANIZACIJA, PERKANTYSIS SUBJEKTAS", document);
        if (id != null && !id.isEmpty()) {
            if (id.contains("(")) {
                id = id.split("\\(")[1].split("\\)")[0].replace(" ", "");
            }
            return id;
        }
        return null;
    }

    /**
     * Parse final price.
     *
     * @param document document to parse from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseFinalPrice(final Element document) {
        String finalPrice = JsoupUtils.selectText(
                "tr:has(i:containsOwn(Bendra galutinė sudarytos)) + tr", document);

        if (finalPrice == null) {
            finalPrice = JsoupUtils.selectText(
                    "tr:has(i:containsOwn(galutinė sutarties (-čių) vertė)) + tr", document);
        }

        if (finalPrice == null || finalPrice.isEmpty()) {
            return null;
        }

        boolean isWithWat = finalPrice.contains("Su PVM");

        final String[] finalPriceParts = finalPrice.split("Valiuta");

        if (finalPrice.length() > 1) {
            final String currency;
            final String vat;
            if (finalPriceParts[1].contains("PVM tarifas")) {
                final String[] currencyVAT = finalPriceParts[1].split("PVM tarifas");

                if (currencyVAT.length > 1) {
                    currency = currencyVAT[0];
                    vat = currencyVAT[1];
                } else if (currencyVAT.length == 1) {
                    currency = currencyVAT[0];
                    vat = null;
                } else {
                    currency = finalPriceParts[1];
                    vat = null;
                }
            } else {
                currency = finalPriceParts[1];
                vat = null;
            }

            if (isWithWat) {
                return new ParsedPrice()
                        .setAmountWithVat(finalPriceParts[0])
                        .setCurrency(currency)
                        .setVat(vat);
            } else {
                return new ParsedPrice()
                        .setNetAmount(finalPriceParts[0])
                        .setCurrency(currency)
                        .setVat(vat);
            }
        } else {
            return null;
        }
    }

    /**
     * Parse first lot.
     *
     * @param document document to parse from.
     * @return ParsedTenderLot or null;
     */
    private static ParsedTenderLot parseLot(final Element document) {
        Element lot = JsoupUtils.selectFirst("td:has(i:containsOwn(Sutartis nr.))", document);

        if (lot != null && lot.textNodes().size() > 1) {
            final List<TextNode> nodes = lot.textNodes();

            return new ParsedTenderLot()
                    .setPositionOnPage("1")
                    .setContractNumber(nodes.get(0).text())
                    .setTitle(nodes.get(1).text())
                    .setBidsCount(parseIinTD("Gautų pasiūlymų skaičius", document))
                    .setAwardDecisionDate(parseIinTD("Sutarties sudarymo data", document))
                    .setBidsCount(parseIinTD("Gautų pasiūlymų skaičius", document))
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .addBidder(parseSupplier(document))
                            .setIsSubcontracted(parseIinTD("Ar tikėtina, kad sutartis bus perduota vykdyti" +
                                    " subrangovams", document))
                            .setPrice(parseLotFinalPrice(document)));
        } else {
            String lotNumber = parseIinTD("Pirkimo dalies numeris", document);
            ParsedBody supplier = null;
            if (lotNumber != null && !lotNumber.isEmpty()) {
                Element supplierInfo = JsoupUtils.selectFirst("table:contains(INFORMACIJA APIE TIEKĖJĄ)", document);
                if (supplierInfo != null) {
                    String rawAddress = parseIinTD("Adresas, telefonas, faksas", supplierInfo);
                    ParsedAddress address = new ParsedAddress().setRawAddress(rawAddress);
                    String phone = null;
                    if (rawAddress != null && !rawAddress.isEmpty()) {
                        String[] data = rawAddress.split(", ");
                        address.setStreet(data[0]);
                        if (data.length > 1) {
                            for (int i = 1; i < data.length; i++) {
                                if (data[i].contains("el.")) {
                                    phone = data[i].split("el.")[1].split(",")[0].replace(" ", "");
                                } else if (data[i].contains("-")) {
                                    String[] cityAndPostcode = data[i].split(" ");
                                    for (int j = 0; j < cityAndPostcode.length; j++) {
                                        if (cityAndPostcode[j].contains("-")) {
                                            address.setPostcode(cityAndPostcode[j]);
                                        } else if (!data[i].contains("ietuva")
                                                && cityAndPostcode[j].equals(cityAndPostcode[j].replaceAll("[0-9]", ""))) {
                                            address.setCity(cityAndPostcode[j]);
                                        } else if (data[i].contains("ietuva")) {
                                            address.setCountry("Lietuva");
                                        }
                                    }
                                } else if (data[i].contains("ietuva")) {
                                    address.setCountry("Lietuva");
                                } else if (data[i].equals(data[i].replaceAll("[0-9]", ""))) {
                                    address.setCity(data[i].replace(" ", ""));
                                }
                            }
                        }
                    }
                    supplier = new ParsedBody()
                            .setName(parseIinTD("Oficialus pavadinimas", supplierInfo))
                            .setAddress(address)
                            .setPhone(phone);
                }
                return new ParsedTenderLot()
                        .setLotNumber(lotNumber)
                        .setTitle(parseNextLine("Pirkimo dalies pavadinimas", document))
                        .addCpv(new ParsedCPV().setIsMain(String.valueOf(true)).setCode(parseIinTD(new String[]{
                                "Pirkimo objekto kodai pagal Bendrą viešųjų pirkimų žodyną", "(angl. CPV)"
                        }, document)))
                        .addBid(new ParsedBid().setIsWinning(String.valueOf(true)).addBidder(supplier));
            } else {
                return null;
            }
        }
    }

    /**
     * Parses info about supplier.
     * @param document document to be parsed
     * @return  parsed body with supplier's info
     */
    private static ParsedBody parseSupplier(final Element document) {
        Element supplierInfo = JsoupUtils.selectFirst("tr:contains(PIRKIMO SUTARTIS (PRELIMINARIOJI SUTARTIS)) ~ tr", document);
        if (supplierInfo != null) {
            Elements tdFields = supplierInfo.select("td.clThinborderall");
            if (tdFields.size() > 1) {
                String id = tdFields.get(0).text();
                String name = tdFields.get(1).text();
                if ((id != null && !id.isEmpty()) || (name != null && !name.isEmpty())) {
                    return new ParsedBody()
                            .setName(name)
                            .addBodyId(new BodyIdentifier()
                                    .setId(id)
                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                    .setScope(BodyIdentifier.Scope.LT));
                }
            }
        } else {
            String bidderNameAndId = parseIinTD("su kuriuo buvo sudaryta sutartis, Oficialus pavadinimas", document);
            String name = null;
            BodyIdentifier bodyId = null;
            if (bidderNameAndId != null && !bidderNameAndId.isEmpty()) {
                String[] parts;
                if (bidderNameAndId.contains(" (")) {
                    parts = bidderNameAndId.split(" \\(");
                } else {
                    parts = bidderNameAndId.split("\\(");
                }
                name = parts[0];
                if (parts.length > 1) {
                    String id = parts[1];
                    if (!id.isEmpty()) {
                        bodyId = new BodyIdentifier()
                                .setId(id)
                                .setScope(BodyIdentifier.Scope.LT)
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID);
                    }
                }
            }
            Element supplierTable = JsoupUtils.selectFirst("table[id^=Table]:contains(INFORMACIJA APIE TIEKĖJĄ)", document);
            if (name != null || bodyId != null) {
                return new ParsedBody().setName(name).addBodyId(bodyId)
                        .setAddress(parseAddress(supplierTable));
            } else {
                return null;
            }
        }
        return null;
    }

    /**
     * Parse lot final price.
     *
     * @param document document to parse from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseLotFinalPrice(final Element document) {
        String finalPrice = parseIinTD("Bendra galutinė sutarties vertė", document);
        if (finalPrice != null) {
            if (finalPrice.isEmpty()) {
                return null;
            }
            boolean isWithWat = finalPrice.contains("Be PVM");
            final String[] finalPriceParts = finalPrice.split("Valiuta");

            if (finalPrice.length() > 1) {
                if (isWithWat) {
                    return new ParsedPrice()
                            .setAmountWithVat(finalPriceParts[0])
                            .setCurrency(finalPriceParts[1].split("Be")[0].replace(" ", ""));
                } else {
                    return new ParsedPrice()
                            .setNetAmount(finalPriceParts[0])
                            .setCurrency(finalPriceParts[1].split("Be")[0].replace(" ", ""));
                }
            } else {
                return null;
            }
        } else {
            finalPrice = parseIinTD("FAKTINĖ PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) VERTĖ", document);
            if (finalPrice != null) {
                if (finalPrice.isEmpty()) {
                    return null;
                } else {
                    Element currencyElem = JsoupUtils
                            .selectFirst("*.containsOwn(FAKTINĖ PIRKIMO SUTARTIES (PRELIMINARIOSIOS SUTARTIES) VERTĖ)", document);

                    String currency = null;
                    if(currencyElem != null){
                        currency = currencyElem.text().contains("EUR") ? "EUR" : "LTL";
                    }
                    return new ParsedPrice().setCurrency(currency).setNetAmount(finalPrice);

                }
            }
        }
        return null;
    }

    /**
     * Parse supply type.
     *
     * @param document document to parse from.
     * @return String or null;
     */
    private static String parseSupplyType(final Element document) {
        String supplyType = parseIinTD("rties tipas ir darbų atlikimo, prekių pristatymo ar paslaugų teikimo v",
                document);

        if (supplyType == null) {
            supplyType = JsoupUtils.selectText("b:containsOwn(Paslaugos)", document);
        }

        return supplyType;
    }

    /**
     * Parse buyer assigned id.
     *
     * @param document document to parse from.
     * @return String or null;
     */
    private static String parseBuyerAssignedId(final Element document) {
        String buyerAssignedId = parseOwnText("td b:has(i:containsOwn(Pirkimo numeris:))", document);

        if (buyerAssignedId == null) {
            buyerAssignedId = parseOwnText("i:containsOwn(numeris)", document);
        }
        if(buyerAssignedId != null){
            buyerAssignedId = buyerAssignedId.split(" ")[0];
        }

        return buyerAssignedId;
    }

    /**
     * Select "i" in "td".
     *
     * @param selector selector
     * @param element  element to parse from
     * @return String or null
     */
    private static String parseIinTD(final String selector, final Element element) {
        String parsedText = null;
        for (String partOfSelector : selector.split(", ")) {
            Element titleElement = JsoupUtils.selectFirst("i:containsOwn(" + partOfSelector + ")", element);
            if (titleElement == null) {
                continue;
            }

            if(titleElement.parent() == null){
                parsedText = "";
            } else {
                parsedText = titleElement.parent().ownText();
            }
            if (parsedText != null) {
                if (parsedText.isEmpty()) {
                    String iValue = titleElement.selectFirst("i:containsOwn(" + partOfSelector + ")").text();
                    if (iValue.split(":").length > 1) {
                        parsedText = iValue.split(":")[1];
                    }
                    titleElement = titleElement.parent().nextElementSibling();
                    if (titleElement == null) {
                        continue;
                    }
                    parsedText = titleElement.text();
                    if (parsedText == null || parsedText.isEmpty()) {
                        continue;
                    }
                }
                break;
            }
        }

        if (parsedText != null && !parsedText.isEmpty() && !parsedText.replace("&nbsp", "").isEmpty()) {
            return parsedText.replace("&nbsp", "");
        } else {
            return null;
        }
    }

    /**
     * Parses and adds CPVs.
     *
     * @param document document to be parsed
     * @return parsed CPVs
     */
    private static List<ParsedCPV> parseCpvs(final Element document) {
        List<ParsedCPV> parsedCpvs = new ArrayList<>();
        String text = null;
        Elements elementsWithText = JsoupUtils.select("td:containsOwn(Pagrindinis objektas) + td", document);
        if (elementsWithText != null) {
            text = elementsWithText.text();
        }
        if (text == null || text.isEmpty()) {
            text = parseIinTD(new String[]{
                    "CPV",
                    "Pirkimo objekto kodai pagal Bendrą viešųjų pirkimų žodyną"
            }, document);
            if (text != null && !text.replaceAll("[^0-9\\-]", "").isEmpty()) {
                if (text.contains("\\")) {
                    text = text.split("\\)")[1];
                }
            } else {
                return null;
            }
            for (String cpvCode : (List<String>) Arrays.asList(text.split(", "))) {
                if (parsedCpvs.isEmpty()) {
                    parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(true)).setCode(cpvCode));
                } else {
                    parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(false)).setCode(cpvCode));
                }
            }
        } else {
            parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(true)).setCode(text.split(" ")[0].split("\\(")[0]));
            Element actualElement = JsoupUtils.select("tr:contains(Pagrindinis objektas)", document).get(0).nextElementSibling();
            while (actualElement != null) {
                parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(false))
                        .setCode(actualElement.select("td:matches(^\\d+.*)").get(0).ownText().split(" ")[0].split("\\(")[0]));
                actualElement = actualElement.nextElementSibling();
            }
        }

        return parsedCpvs;
    }

    /**
     * Select "i" in "td".
     *
     * @param selectors selector
     * @param element   element to parse from
     * @return String or null
     */
    private static String parseIinTD(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = parseIinTD(selector, element);
            if (result != null) {
                return result;
            }
        }
        return null;
    }
}
