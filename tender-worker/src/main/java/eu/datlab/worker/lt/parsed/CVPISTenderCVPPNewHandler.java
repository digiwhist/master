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
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.datlab.worker.lt.parsed.CVPISTenderParser.parseOwnText;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Created by michalriha on 05/06/2017.
 */
public final class CVPISTenderCVPPNewHandler {

    /**
     * Private constructor.
     */
    private CVPISTenderCVPPNewHandler() {
    }

    /**
     * Parse new form.
     *
     * @param document        document to be parsed
     * @param url             url
     * @param publicationDate date of publishing of the tender
     * @return parsed tender
     */
    public static ParsedTender parse(final Element document, final String url, final String publicationDate) {
        return new ParsedTender()
                .setBuyerAssignedId(parseTrUnderTr("priskirtas bylos numeris",
                        document))
                .addBuyer(new ParsedBody()
                        .setName(parseName(document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseBodyId(document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.LT)
                        )
                        .setPhone(parseTextNodeUnder("telefonas, Telefonas", document))
                        .setAddress(parseAddress(document))
                        .setEmail(parseEmail(document))
                        .setContactPoint(parseContactPoint(document))
                        .setContactName(parseContactPoint(document))
                        .setBuyerType(selectText("table:contains(Perkančiosios organizacijos ti" +
                                "pa) + table", document))
                        .addMainActivity(selectText("table:contains(Pagrindinė veikla)" +
                                " + table", document))
                )
                .setIsOnBehalfOf(parseChecked("td:containsOwn(Perkančioji organizacija perka kitų perkančiųjų " +
                        "organizacijų vardu:) + td", document))
                .setTitle(parseTitle(document))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(parseBinTD(new String[]{
                                "Pagrindinė pristatymo vieta",
                                "atlikimo, prekių pristatymo ar paslaugų teikimo",
                                "agrindin� teikimo viet"
                        }, document))
                        .addNuts(parseBinTD("NUTS", document))
                )
                .setDescription(selectText("tr:contains(Trumpas sutarties ar pirkimo) + tr",
                        document))
                .setCpvs(parseCpvs(document))
                .setIsCoveredByGpa(parseBinTD("(GPA)", document))
                .setHasLots(parseBinTD("Pirkimo dalijimas", document))
                .setAreVariantsAccepted(parseBinTD("ma pateikti alternatyvius pa", document))
                .setEstimatedDurationInMonths(parseBinTD("ties trukm� arba �vykdymo termi", document))
                .setDeposits(parseBinTD(new String[]{
                        "os sąlygos.Sutarties įvykdymo užti",
                        "statai ir garantijos:"
                }, document))
                .setPersonalRequirements(parseBinTD("Informacija apie asmenin", document))
                .setEconomicRequirements(parseBinTD("Informacija apie asmenin", document))
                .setTechnicalRequirements(parseBinTD("Informacija apie asmenin", document))
                .setProcedureType(parseProcedureType(document))
                .setSelectionMethod(parseSelectionMethod(document))
                .setIsElectronicAuction(parseChecked("tr:has(td:contains(elektroninis aukcionas)) + tr > td",
                        document))
                .setDocumentsPayable(parseBinTD("mamas mokest", document))
                .setBidDeadline(parseBidDeadline(document))
                .addEligibleBidLanguage(parseBinTD("ai leisti dalyvauti", document))
                .setAwardDeadlineDuration(parseBinTD("rivalo u�tikrinti pasi�lymo galiojim� (atvira proced�ra)",
                        document))
                .setIsFrameworkAgreement(isFrameWorkAgreement(document))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(parseChecked("td:contains(is yra susijusi su projektu ir (arba) progr" +
                                "ama, finansuo) + td", document))
                )
                .setSupplyType(parseChecked("tr:contains(Sutarties tipas ir darbų atlikimo, prekių prist" +
                        "atymo ar paslaugų teikim) + tr", document))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(PublicationSources.LT_CVPIS)
                        .setHumanReadableUrl(url)
                        .setSourceFormType(parseSourceFormType(document))
                        .setDispatchDate(selectText("table:contains(Šio skelbimo išsiuntimo data) + table", document))
                        .setPublicationDate(publicationDate)
                        .setSourceTenderId(parseTrUnderTr("Perkančiosios organizacijos priskirtas bylos numeris",
                                document)))
                .setIsDps(isDPS(document))
                .setFinalPrice(parseFinalPrice(document))
                .setLots(parseLots(document))
                .addPublication(parsePreviousPublication(document));
    }

    /**
     * Parses title.
     * @param document document to be parsed
     * @return parsed title
     */
    private static String parseTitle(final Element document){
        String[] selector = new String[]{
                "Sutarčiai suteiktas pavad",
                "adinimas, kurį perkančioji organizacija (institucija) suteikė sutar",
                "ar perkantysisi subjektas sutei",
                "rkimo pavadinimas",
                "rganizacija (institucija) suteik� sutar�iai"
        };
        String title = parseBinTD(selector, document);
        if(title != null){
            return title;
        }
        for(String partOfSelector: selector){
            Element nextTr = JsoupUtils.selectFirst("b:containsOwn(" + partOfSelector + ")", document);
            if(nextTr == null){
                continue;
            }
            nextTr = nextTr.parent().parent().nextElementSibling();
            if(nextTr != null){
                nextTr = nextTr.selectFirst("td");
                if(nextTr == null){
                    return null;
                }
                title = nextTr.ownText();
                return title;
            }
        }
        return null;
    }

    /**
     * Parses bid deadline.
     * @param document document to be parsed
     * @return parsed bid deadline
     */
    private static String parseBidDeadline(final Element document){
        String deadline = parseBinTD("si�lym� ar pra�ym� leisti dalyvauti pri�mimo terminas", document);
        if(deadline == null){
            Element deadlineTitle = JsoupUtils.selectFirst("tr:contains( Pasiūlymų (pirminių pasiūlymų) " +
                    "pateikimo terminas) + tr", document);
            if(deadlineTitle == null){
                return null;
            }
            deadlineTitle = deadlineTitle.selectFirst("td");
            if(deadlineTitle == null){
                return null;
            }
            deadline = deadlineTitle.ownText();
            String tmp = deadline;
            deadline = "";
            for(String part: tmp.split(" ")){
                if(Character.isDigit(part.charAt(0))){
                    deadline = deadline.concat(part + " ");
                }
            }
            if(!deadline.isEmpty()){
                deadline = deadline.substring(0, deadline.length() - 1);
            }
        }
        return deadline;
    }

    /**
     * Parses selection method.
     * @param document document to be parsed
     * @return parsed selection method
     */
    private static String parseSelectionMethod(final Element document){
        String method = parseTrUnderTr("IV.2.1\\) Sutarties sudarymo kriterijai", document);
        if(method == null){
            Element methodElem = JsoupUtils.selectFirst("b:containsOwn(IV.2.1. Pasiūlymų vertinimo kriterijai)",
                    document);
            if(methodElem == null){
                return null;
            }
            methodElem = methodElem.parent().parent().parent();
            if(methodElem == null){
                return null;
            }
            for(Element tr: methodElem.select("tr")){
                Element input = tr.selectFirst("input");
                if(input != null && input.hasAttr("checked")){
                    method = input.parent().ownText();
                }
            }
        }
        return method;
    }

    /**
     * Parses source form type.
     *
     * @param document document to be parsed
     * @return parsed source form type
     */
    private static String parseSourceFormType(final Element document) {
        String formType = selectText("*:containsOwn(skelbimas), *:containsOwn(SKELBIMAS)", document);
        if (formType == null || formType.isEmpty()) {
            Element elementWithForm = document.selectFirst("table").selectFirst("*:containsOwn(form)");
            if (elementWithForm != null) {
                for (String part : elementWithForm.ownText().split(" ")) {
                    if (part.contains("-")) {
                        return part;
                    }
                }
            }
        } else {
            return formType;
        }
        return null;
    }

    /**
     * Parses body id.
     *
     * @param document document to be parsed
     * @return parsed body id
     */
    private static String parseBodyId(final Element document) {
        String name = parseTextNodeUnder(new String[]{
                "Oficialus pavadinimas"//,  "IOJI ORGANIZACIJA"
        }, document);
        if (name == null || name.isEmpty()) {
            Element nameElem = JsoupUtils.selectFirst("b:containsOwn(Oficialus pavadinimas)", document);
            if (nameElem != null) {
                nameElem = nameElem.parent();
                name = nameElem.ownText();
            } else {
                nameElem = JsoupUtils.selectFirst("b:containsOwn(Organizacijos pavadinimas ir kodas)", document);
                if (nameElem != null) {
                    nameElem = nameElem.parent();
                    name = nameElem.ownText();
                }
            }
        }

        if (name != null && name.contains("(")) {
            return name.split("\\(")[1].split("\\)")[0];
        } else {
            return null;
        }
    }

    /**
     * Parse previouse publication source form type.
     *
     * @param document document to parse from
     * @return String or null
     */
    private static ParsedPublication parsePreviousPublication(final Element document) {
        final Elements formTypes = JsoupUtils.select("table:contains(Ankstesnis\\(-i\\) skelbimas\\(-ai\\) a) " +
                "+ table > tbody > tr", document);

        if (formTypes == null || formTypes.isEmpty()) {
            return null;
        }

        String sourceFormType = null;
        boolean nextSourceId = false;
        String sourceIdAndPublishDate = null;

        for (Element formType : formTypes) {
            if (isOrItsChildChecked(formType)) {
                final Element result = JsoupUtils.selectFirst("td", formType);
                if (result != null) {
                    sourceFormType = result.text();
                    nextSourceId = true;
                }
            } else if (nextSourceId) {
                final Element result = JsoupUtils.selectFirst("td", formType);
                if (result != null && result.textNodes().size() > 1) {
                    sourceIdAndPublishDate = result.textNodes().get(1).text();
                    nextSourceId = false;
                }
            }
        }

        return new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.LT_CVPIS)
                .setSourceFormType(sourceFormType)
                .setSourceId(sourceIdAndPublishDate)
                .setPublicationDate(sourceIdAndPublishDate);
    }

    /**
     * Parse isFrameWorkAgreement.
     *
     * @param document document to parse from
     * @return String or null
     */
    private static String isDPS(final Element document) {
        Boolean isDPS = isChecked(JsoupUtils.selectFirst("td:containsOwn(Skelbimas susijęs su sutartimi" +
                "(-imis), grindžiama(-omis) dinamine pirkimo sistema (DPS) ) + td input", document));

        if(isDPS == null){
            Element dpsTr = JsoupUtils.selectFirst("b:containsOwn(Ar pirkimas atliekamas taikant dinaminę pirkimų sistemą)", document);
            if(dpsTr == null){
                return null;
            }
            dpsTr = dpsTr.parent().parent().nextElementSibling();
            if(dpsTr == null){
                return null;
            }

            Element input1 = dpsTr.selectFirst("input");
            if(input1 != null) {
                if (input1.hasAttr("checked")) {
                    isDPS = Boolean.TRUE;
                } else {
                    isDPS = Boolean.FALSE;
                }
            } else {
                isDPS = null;
            }
        }

        return isDPS == null ? null : isDPS.toString();
    }

    /**
     * Parse isFrameWorkAgreement.
     *
     * @param document document to parse from
     * @return String or null
     */
    private static String isFrameWorkAgreement(final Element document) {
        final Boolean isFrameWorkAgreement = isChecked(JsoupUtils.selectFirst("td:containsOwn(Skelbimas susijęs su " +
                "preliminariojo susitarimo sudarymu) + td input", document));

        return isFrameWorkAgreement == null ? null : isFrameWorkAgreement.toString();
    }

    /**
     * Parse buyers Url.
     *
     * @param document document to parse from
     * @return String or null
     */
    private static String parseBuyerUrl(final Element document) {
        final Element url = JsoupUtils.selectFirst("td:contains(Interneto adresas)", document);

        if (url != null && url.text().contains("http") && url.text().contains(".lt")) {
            return "http" + url.text().split("http")[1].split(".lt")[0] + ".lt";
        } else {
            return null;
        }
    }

    /**
     * Parse final price.
     *
     * @param document document to parse from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseFinalPrice(final Element document) {
        final Element finalPriceRow = JsoupUtils.selectFirst("tr:contains(Bendra galutinė sutarties (-čių) ver) + tr",
                document);


        if (finalPriceRow == null) {
            return null;
        }

        final String finalPrice = selectText("td:containsOwn(Vertė)", finalPriceRow);


        if (finalPrice != null) {
            final Boolean withVat = isChecked(JsoupUtils.selectFirst("input#Checkbox2", finalPriceRow));

            if (withVat == null || !withVat) {
                return new ParsedPrice()
                        .setNetAmount(finalPrice)
                        .setVat(selectText("td:containsOwn(21)", finalPriceRow))
                        .setCurrency(selectText("td:containsOwn(Valiuta)", finalPriceRow));
            } else {
                return new ParsedPrice()
                        .setAmountWithVat(finalPrice)
                        .setVat(selectText("td:containsOwn(21)", finalPriceRow))
                        .setCurrency(selectText("td:containsOwn(Valiuta)", finalPriceRow));
            }
        } else {
            return null;
        }
    }

    /**
     * Parse lots.
     *
     * @param document document to parse from.
     * @return ParsedTenderLot or null;
     */
    private static List<ParsedTenderLot> parseLots(final Element document) {
        final Elements lotFirstLines = JsoupUtils.select("table:contains(Sutartis Nr.)", document);
        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            parsedLots.add(new ParsedTenderLot().setLotNumber("1"));
            return parsedLots;
        }

        List<Element> lots = new ArrayList<>();

        for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
            if ((iterator + 1) != lotFirstLines.size()) {
                lots.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator),
                        lotFirstLines.get(iterator + 1)));
            } else {
                lots.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null));
            }
        }

        int lotCounter = 1;
        for (Element lot : lots) {
            lot.text();
            parsedLots.add(new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(lotCounter++))
                    .setContractNumber(parseBinTD("Sutartis Nr", lot))
                    .setTitle(parseBinTD("Pirkimo dalies pavadinimas", lot))
                    .setBidsCount(selectText("*:containsOwn(Gautų pasiūlymų skaičiu)", document))
                    .setElectronicBidsCount(selectText("*:containsOwn(Elektroninėmis priemonėmis gautų" +
                            " pasiūl)", document))
                    .setAwardDecisionDate(parseBinTD("prendimo dėl sutarties sudarymo ", lot))
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .addBidder(new ParsedBody()
                                    .setName(parseTextNodeUnder("Oficialus pavadinimas", document))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(parseTextNodeUnder("Oficialus pavadinimas", document))
                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                            .setScope(BodyIdentifier.Scope.LT))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(parseTextNodeUnder("Adresas", document))
                                            .setCity(parseTextNodeUnder("Miestas", document))
                                            .setPostcode(parseTextNodeUnder("Pašto kodas", document))
                                            .setCountry(parseTextNodeUnder("Šalis", document))
                                    )
                                    .setPhone(parseTextNodeUnder("Telefonas", document))
                            )
                            .setIsSubcontracted(parseChecked("td:contains(Informacija apie subran" +
                                    "gos sutarčių sudarymą) + td", document))
                            .setPrice(parseLotFinalPrice(document))
                    ));
        }

        return parsedLots;
    }

    /**
     * Parse lot final price.
     *
     * @param document document to parse from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseLotFinalPrice(final Element document) {
        final Element finalPriceRow = JsoupUtils.selectFirst("tr:contains(Bendra galutinė sutarties vertė)", document);

        if (finalPriceRow == null) {
            return null;
        }

        final String finalPrice = selectText("td:containsOwn(Vertė)", finalPriceRow);

        if (finalPrice != null) {
            final Boolean withVat = isChecked(JsoupUtils.selectFirst("input#Checkbox2", finalPriceRow));

            if (withVat == null || !withVat) {
                return new ParsedPrice()
                        .setNetAmount(finalPrice)
                        .setVat(selectText("td:containsOwn(21)", finalPriceRow))
                        .setCurrency(selectText("td:containsOwn(Valiuta)", finalPriceRow));
            } else {
                return new ParsedPrice()
                        .setAmountWithVat(finalPrice)
                        .setVat(selectText("td:containsOwn(21)", finalPriceRow))
                        .setCurrency(selectText("td:containsOwn(Valiuta)", finalPriceRow));
            }
        } else {
            return null;
        }
    }

    /**
     * Select TDs own text witcha also contains b with text in selector.
     *
     * @param selector selector
     * @param element  element to parse from
     * @return String or null
     */
    private static String parseBinTD(final String selector, final Element element) {
        for (String partOfSelector : selector.split(", ")) {
            String value = parseOwnText("td:has(b:containsOwn(" + partOfSelector + "))", element);
            if (value != null) {
                return value;
            }
        }
        return null;
    }

    /**
     * Select "b" in "td".
     *
     * @param selectors selector
     * @param element   element to parse from
     * @return String or null
     */
    private static String parseBinTD(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = parseBinTD(selector, element);

            if (result != null && !result.isEmpty()) {
                return result;
            }
        }

        return null;
    }


    /**
     * Parses address.
     *
     * @param document document to be parsed
     * @return parsed address
     */
    private static ParsedAddress parseAddress(final Element document) {
        ParsedAddress parsedAddress = new ParsedAddress();
        Element tableWithData = JsoupUtils.selectFirst("table:contains(DALIS: PERKANČIOJI ORGANIZACIJA) ~ table", document);
        if(tableWithData == null){
            return null;
        }
        if (JsoupUtils.selectFirst("*:contains(Adresas)", tableWithData) == null) {
            tableWithData = tableWithData.nextElementSibling();
            if (tableWithData == null) {
                return null;
            }
        }
        String address = parseTextNodeUnder("Adresas", tableWithData);
        if (address != null) {
            parsedAddress
                    .setRawAddress(address)
                    .setCity(parseTextNodeUnder("Miestas", tableWithData))
                    .setCountry(parseTextNodeUnder("Šalis", tableWithData))
                    .setPostcode(parseTextNodeUnder("Pašto kodas, Pašto indeksas", tableWithData))
                    .setUrl(parseBuyerUrl(document));
        }
        return parsedAddress;
    }

    /**
     * Parses procedure type.
     *
     * @param document document to be parsed
     * @return parsed procedure type
     */
    private static String parseProcedureType(final Element document) {
        String procedureType = parseTrUnderTr("Procedūros tipas", document);
        if (procedureType == null) {
            Element withTdElements = JsoupUtils.selectFirst("tr:contains(Procedūros rūšis) ~ tr", document);
            if (withTdElements != null) {
                withTdElements = withTdElements.selectFirst("td");
            }
            if (withTdElements != null) {
                if(withTdElements.selectFirst("tr") == null){
                    return withTdElements.ownText();
                }
                withTdElements = withTdElements.selectFirst("tr");
            }

            Elements tdElements = null;
            if (withTdElements != null) {
                tdElements = withTdElements.select("td");
            }
            if (tdElements == null || tdElements.isEmpty()) {
                return null;
            }

            if (tdElements.size() > 1) {
                procedureType = tdElements.get(1).ownText();
            } else {
                procedureType = tdElements.get(0).ownText();
            }
        }
        if (Character.isDigit(procedureType.charAt(0)) && procedureType.length() > 3) {
            procedureType = procedureType.substring(3);
        }
        return procedureType;
    }

    /**
     * Parses email.
     *
     * @param document document to be parsed
     * @return parsed email
     */
    private static String parseEmail(final Element document) {
        String email = parseBinTD("El.paštas", document);
        if (email == null) {
            Element emailElem = JsoupUtils.selectFirst("td:containsOwn(El. paštas)", document);
            if (emailElem != null) {
                email = emailElem.ownText();
                if (email.startsWith("El. paštas") && email.length() > "El. paštas".length()) {
                    email = email.split("El. paštas")[1];
                    email = email.replace(" ", "").replace(":", "");
                }
            }
        }
        return email;
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
            elementsWithText = JsoupUtils.select("td:contains(Pirkimo " +
                    "objekto kodai pagal Bendrą viešųjų pirkimų žodyną), td:containsOwn(CPV)", document);
            if (elementsWithText != null) {
                text = elementsWithText.text();
                if (text.contains("\\")) {
                    text = text.split("\\)")[1];
                }
            } else {
                return null;
            }
            for (String cpvCode : (List<String>) Arrays.asList(text.split(", "))) {
                if (!cpvCode.isEmpty()) {
                    parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(false)).setCode(cpvCode));
                }
            }
        } else {
            parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(true)).setCode(text.split(" ")[0].split("\\(")[0]));
            Element actualElement = ((Elements) JsoupUtils
                    .select("tr:contains(Pagrindinis objektas)", document)).last().nextElementSibling();
            while (actualElement != null && actualElement.selectFirst("td:matches(^\\d+.*)") != null) {
                parsedCpvs.add(new ParsedCPV().setIsMain(String.valueOf(false))
                        .setCode(actualElement.select("td:matches(^\\d+.*)")
                                .get(0).ownText().split(" ")[0].split("\\(")[0]));
                actualElement = actualElement.nextElementSibling();
            }
        }
        if (parsedCpvs.isEmpty()) {
            return null;
        }
        return parsedCpvs;
    }

    /**
     * Parses buyer's name.
     *
     * @param document document to be parsed
     * @return Buyer's name
     */
    private static String parseName(final Element document) {
        String name = parseTextNodeUnder(new String[]{
                "Oficialus pavadinimas"//, "IOJI ORGANIZACIJA",
        }, document);
        if (name == null) {
            Element nameElem = JsoupUtils.selectFirst("b:containsOwn(Oficialus pavadinimas)", document);
            if (nameElem != null) {
                nameElem = nameElem.parent();
                name = nameElem.ownText();
            } else {
                nameElem = JsoupUtils.selectFirst("b:containsOwn(Organizacijos pavadinimas ir kodas)", document);
                if (nameElem != null) {
                    nameElem = nameElem.parent();
                    name = nameElem.ownText();
                }
            }
        }
        if (name != null && name.contains("(")) {
            name = name.split("\\(")[0];
        }

        if (name != null && name.isEmpty()) {
            return null;
        }
        return name;
    }

    /**
     * Parses contact point.
     *
     * @param document document to be parsed.
     * @return contact name.
     */
    private static String parseContactPoint(final Element document) {
        String contactPoint = parseTextNodeUnder("Kontaktinis(-iai) punktas(-ai)", document);
        if (contactPoint == null) {
            Element contactPointElem = JsoupUtils.selectFirst("b:containsOwn(Kontaktiniai duomenys):not(b:containsOwn(1.))", document);
            if (contactPointElem != null) {
                contactPointElem = contactPointElem.parent();
                contactPoint = contactPointElem.ownText();
                if (contactPoint.startsWith("Kam") && contactPoint.length() > "Kam".length()) {
                    contactPoint = contactPoint.split("Kam")[1].replace(":", "");
                }
            }
        }
        return contactPoint;
    }

    /**
     * Select text node under text node with text in selector.
     *
     * @param selector selector
     * @param element  element to parse from
     * @return String or null
     */
    private static String parseTextNodeUnder(final String selector, final Element element) {
        Element result = null;
        for (String partOfSelector : selector.split(", ")) {
            if(partOfSelector.contains("I ")) {
                result = JsoupUtils.selectFirst("*:containsOwn(" + partOfSelector + ")", element);
            } else {
                result = JsoupUtils.selectFirst("*:containsOwn(" + partOfSelector + "):not(*:containsOwn(I ))", element);
            }
            if (result != null) {
                if(result.selectFirst("br") == null) {
                    result = result.parent();
                }
                break;
            }
        }
        String text = null;
        if (result == null) {
            return null;
        }
        if (result.textNodes().size() > 1 && !result.textNodes().get(1).text().replaceAll(" ", "").isEmpty()) {
            text = result.textNodes().get(1).text();
        } else {
            text = result.ownText();
        }
        if (text != null && !text.isEmpty()) {
            return text;
        }
        return null;
    }

    /**
     * Select text node under text node with text in selector.
     *
     * @param selectors selector
     * @param element   element to parse from
     * @return String or null
     */
    private static String parseTextNodeUnder(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = parseTextNodeUnder(selector, element);

            if (result != null && !result.isEmpty()) {
                return result;
            }
        }

        return null;
    }

    /**
     * Select tr under tr with text in selector.
     *
     * @param selector selector
     * @param element  element to parse from
     * @return String or null
     */
    private static String parseTrUnderTr(final String selector, final Element element) {
        Element result = null;
        for(String partOfSelector: selector.split(", ")) {
            result = JsoupUtils.selectFirst("tr:contains(" + partOfSelector + ") + tr", element);
            if(result != null){
                break;
            }
        }

        if (result != null) {
            return result.text();
        } else {
            return null;
        }
    }

    /**
     * Parse checked value.
     *
     * @param selector selector to get parent of inputs
     * @param element  element to parse from
     * @return String or null
     */
    private static String parseChecked(final String selector, final Element element) {
        final Element elementWithInputs = JsoupUtils.selectFirst(selector, element);

        if (elementWithInputs == null) {
            return null;
        }

        final List<Node> nodes = elementWithInputs.childNodes();

        for (int i = 2; i < nodes.size(); i++) {
            if (isOrItsChildChecked(nodes.get(i))) {
                if(isChecked(nodes.get(i))) {
                    return nodes.get(i-1).toString().replace("&nbsp;", "");
                } else {
                    for(Node child: nodes.get(i).childNodes()){
                        if(child.nextSibling() != null && isChecked(child.nextSibling())){
                            return child.toString().replace("&nbsp;", "");
                        }
                    }
                }
            }
        }

        return null;
    }

    /**
     * Has checked attribute itself or one of its child nodes.
     *
     * @param node node to search attribute in
     * @return boolean
     */
    private static boolean isOrItsChildChecked(final Node node) {
        try {
            return isChecked(node) || (!node.childNodes().isEmpty() && hasCheckedChild(node.childNodes()));
        } catch (UnsupportedOperationException ex) {
            // leaf nodes (Comment, DataNode, DocumentType, TextNode and XmlDeclaration) throw an exception when childNodes() method is
            // called.
            return false;
        }
    }

    /**
     * Looks recursively for node with attribute checked.
     *
     * @param nodes nodes to search attribute in
     * @return boolean
     */
    private static boolean hasCheckedChild(final List<Node> nodes) {
        for (Node node : nodes) {
            if (isChecked(node)) {
                return true;
            } else if (!node.childNodes().isEmpty() && hasCheckedChild(node.childNodes())) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checkes if node has checked attribute.
     *
     * @param node node to search attribute in
     * @return Boolean or null
     */
    private static Boolean isChecked(final Node node) {
        if (node == null) {
            return null;
        }

        return node.hasAttr("checked");
    }
}
