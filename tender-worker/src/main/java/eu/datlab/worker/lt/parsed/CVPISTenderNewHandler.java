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
import java.util.List;

import static eu.datlab.worker.lt.parsed.CVPISTenderParser.parseOwnText;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Created by michalriha on 05/06/2017.
 */
public final class CVPISTenderNewHandler {

    /**
     * Private constructor.
     */
    private CVPISTenderNewHandler() {
    }

    /**
     * Parse new form.
     *
     * @param document document to be parsed
     * @param url url
     * @param publicationDate
     *      date of publishing of the tender
     * @return parsed tender
     */
    public static ParsedTender parse(final Element document, final String url, final String publicationDate) {
        ParsedTender parsedTender = new ParsedTender()
                .setBuyerAssignedId(parseTrUnderTr("Perkančiosios organizacijos priskirtas bylos numeris:",
                        document))
                .addBuyer(new ParsedBody()
                        .setName(parseTextNodeUnder(new String[]{
                                "Oficialus pavadinimas:",
                                "IOJI ORGANIZACIJA"
                        }, document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseTextNodeUnder(new String[]{
                                        "Oficialus pavadinimas:",
                                        "IOJI ORGANIZACIJA"
                                }, document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.LT)
                        )
                        .setPhone(parseBinTD("Adresas, telefonas, faks", document))
                        .setAddress(new ParsedAddress()
                                .setStreet(parseTextNodeUnder("Adresas:", document))
                                .setCity(parseTextNodeUnder("Miestas", document))
                                .setCountry(parseTextNodeUnder("Šalis", document))
                                .setPostcode(parseTextNodeUnder("Pašto kodas:", document))
                                .setUrl(parseBuyerUrl(document))
                        )
                        .setEmail(parseBinTD("El.pa�tas", document))
                        .setContactPoint(parseTextNodeUnder("Kontaktinis(-iai) punktas(-ai):", document))
                        .setPhone(parseTextNodeUnder("Telefonas:", document))
                        .setBuyerType(selectText("table:contains(Perkančiosios organizacijos ti" +
                                "pa) + table", document))
                        .addMainActivity(selectText("table:contains(Pagrindinė veikla)" +
                                " + table", document))
                )
                .setIsOnBehalfOf(parseChecked("td:containsOwn(Perkančioji organizacija perka kitų perkančiųjų " +
                        "organizacijų vardu:) + td", document))
                .setTitle(parseBinTD(new String[]{
                        "Sutarčiai suteiktas pavad",
                        "adinimas, kurį perkančioji organizacija (institucija) suteikė sutar",
                        "ar perkantysisi subjektas sutei",
                        "rkimo pavadinimas",
                        "rganizacija (institucija) suteik� sutar�iai:"
                }, document))
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
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(selectText("td:containsOwn(Pagrindinis objektas) + td", document))
                )
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
                .setProcedureType(parseTrUnderTr("IV.1.1\\) Procedūros tipas", document))
                .setSelectionMethod(parseTrUnderTr("IV.2.1\\) Sutarties sudarymo kriterijai", document))
                .setIsElectronicAuction(parseChecked("td:contains(Surengtas elektroninis aukcionas) + td", document))
                .setDocumentsPayable(parseBinTD("mamas mokest", document))
                .setBidDeadline(parseBinTD("si�lym� ar pra�ym� leisti dalyvauti pri�mimo terminas", document))
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
                        .setSourceFormType(selectText("*:containsOwn(skelbimas)", document))
                        .setDispatchDate(selectText("b:contains(Šio skelbimo išsiuntimo data) + table", document))
                        .setPublicationDate(publicationDate)
                )
                .setIsDps(isDPS(document))
                .setFinalPrice(parseFinalPrice(document))
                .setLots(parseLots(document))
                .addPublication(parsePreviousePublication(document)
                );


        return parsedTender;
    }

    /**
     * Parse previouse publication source form type.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private static ParsedPublication parsePreviousePublication(final Element document) {
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
     *
     * @return String or null
     */
    private static String isDPS(final Element document) {
        final Boolean isDPS = isChecked(JsoupUtils.selectFirst("td:containsOwn(Skelbimas susijęs su sutartimi" +
                "(-imis), grindžiama(-omis) dinamine pirkimo sistema (DPS) ) + td input", document));

        return isDPS == null ? null : isDPS.toString();
    }

    /**
     * Parse isFrameWorkAgreement.
     *
     * @param document document to parse from
     *
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
     *
     * @return String or null
     */
    private static String parseBuyerUrl(final Element document) {
        final Element url = JsoupUtils.selectFirst("td:containsOwn(Interneto adresas(-ai): )", document);

        if (url != null && url.textNodes().size() > 2) {
            return url.textNodes().get(2).text();
        } else {
            return null;
        }
    }

    /**
     * Parse final price.
     *
     * @param document document to parse from
     *
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
     *
     * @return ParsedTenderLot or null;
     */
    private static List<ParsedTenderLot> parseLots(final Element document) {
        final Elements lotFirstLines = JsoupUtils.select("table:contains(Sutartis Nr.:)", document);

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            return null;
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

        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

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
                                    .setName(parseTextNodeUnder("Oficialus pavadinimas:", document))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(parseTextNodeUnder("Oficialus pavadinimas:", document))
                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                            .setScope(BodyIdentifier.Scope.LT))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(parseTextNodeUnder("Adresas:", document))
                                            .setCity(parseTextNodeUnder("Miestas:", document))
                                            .setPostcode(parseTextNodeUnder("Pašto kodas:", document))
                                            .setCountry(parseTextNodeUnder("Šalis:", document))
                                    )
                                    .setPhone(parseTextNodeUnder("Telefonas:", document))
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
     *
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
     * @param element element to parse from
     *
     * @return String or null
     */
    private static String parseBinTD(final String selector, final Element element) {
        return parseOwnText("td:has(b:containsOwn(" + selector + "))", element);
    }

    /**
     * Select "b" in "td".
     *
     * @param selectors selector
     * @param element element to parse from
     *
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
     * Select text node under text node with text in selector.
     *
     * @param selector selector
     * @param element element to parse from
     *
     * @return String or null
     */
    private static String parseTextNodeUnder(final String selector, final Element element) {
        final Element result = JsoupUtils.selectFirst("*:containsOwn(" + selector + ")", element);

        if (result != null && result.textNodes().size() > 1) {
            return result.textNodes().get(1).text();
        } else {
            return null;
        }
    }

    /**
     * Select text node under text node with text in selector.
     *
     * @param selectors selector
     * @param element element to parse from
     *
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
     * @param element element to parse from
     *
     * @return String or null
     */
    private static String parseTrUnderTr(final String selector, final Element element) {
        final Element result = JsoupUtils.selectFirst("tr:contains(" + selector + ") + tr", element);

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
     * @param element element to parse from
     *
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
                return nodes.get(i - 2).toString();
            }
        }

        return null;
    }

    /**
     * Has checked attribute itself or one of its child nodes.
     *
     * @param node node to search attribute in
     *
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
     *
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
     *
     * @return Boolean or null
     */
    private static Boolean isChecked(final Node node) {
        if (node == null) {
            return null;
        }

        return node.hasAttr("checked");
    }
}
