package eu.digiwhist.worker.lt.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
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

import java.util.List;

import static eu.digiwhist.worker.lt.parsed.CVPISTenderParser.parseOwnText;

/**
 * Created by michalriha on 05/06/2017.
 */
public final class CVPISTenderOldHandler {

    /**
     * Private constructor.
     */
    private CVPISTenderOldHandler() {
    }

    /**
     * Parse old form.
     *
     * @param document document to be parsed
     * @param url url
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final Element document, final String url) {
        ParsedTender parsedTender = new ParsedTender()
                .setBuyerAssignedId(parseBuyerAssignedId(document))
                .addBuyer(new ParsedBody()
                        .setName(parseIinTD("IOJI ORGANIZACIJA", document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseIinTD("IOJI ORGANIZACIJA", document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.LT)
                        )
                        .setPhone(parseIinTD("Adresas, telefonas, faks", document))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseIinTD("Adresas, telefonas, faks", document))
                                .setUrl(parseIinTD(new String[]{
                                        "ančiojo subjekto adresas",
                                        "Interneto adresas"
                                }, document))
                        )
                        .setEmail(parseIinTD("El.pa�tas", document))
                        .setContactPoint(parseIinTD("Kam", document))
                        .setBuyerType(parseIinTD(new String[]{
                                "Perkančiosios organizacijos tipa",
                                "veiklos sritis ar sritys:"
                        }, document))
                        .addMainActivity(parseIinTD(new String[]{
                                "Pagrindinė perkančiojo subjekto veiklos sritis ar s",
                                "veiklos sritis ar sritys:"
                        }, document))
                )
                .setIsOnBehalfOf(parseIinTD("ioji organizacija (institucija) perka kit", document))
                .setTitle(parseIinTD(new String[]{
                        "adinimas, kurį perkančioji organizacija (institucija) suteikė sutar",
                        "ar perkantysisi subjektas sutei",
                        "rkimo pavadinimas",
                        "rganizacija (institucija) suteik� sutar�iai:"
                }, document))
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
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(parseIinTD(new String[]{
                                "žodynas (BVPŽ)",
                                "Bendrą viešųjų pirkimų žodyną",
                                "(BVP�)"
                        }, document))
                )
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
                .setProcedureType(parseIinTD(new String[]{
                        "rocedūros rūšis",
                        "Proced�ros r��is:"
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
                        .setSourceFormType(JsoupUtils.selectText("*:containsOwn(skelbimas)", document))
                        .setDispatchDate(parseIinTD(new String[]{
                                "Šio skelbimo išsiuntimo dat",
                                "o skelbimo i�siuntimo data"
                        }, document))
                )
                .setFinalPrice(parseFinalPrice(document))
                .addLot(parseLot(document))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(false)
                        .setSource(PublicationSources.LT_CVPIS)
                        .setSourceFormType(JsoupUtils.selectText("i:containsOwn(Skelbimas apie pirkimą)", document))
                        .setSourceId(parseIinTD("Skelbimo OL numeri", document))
                        .setPublicationDate(parseIinTD("Skelbimo OL numeri", document))
                );

        return parsedTender;
    }

    /**
     * Parse final price.
     *
     * @param document document to parse from
     *
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseFinalPrice(final Element document) {
        String finalPrice = JsoupUtils.selectText(
                "tr tr:has(i:containsOwn(Bendra galutinė sudarytos)) + tr", document);

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
                } else if (currencyVAT.length > 1) {
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
     *
     * @return ParsedTenderLot or null;
     */
    private static ParsedTenderLot parseLot(final Element document) {
        Element lot = JsoupUtils.selectFirst("td td:has(i:containsOwn(Sutartis nr.:))", document);

        if (lot == null) {
            lot = JsoupUtils.selectFirst("td:has(i:containsOwn(Sutartis nr.:))", document);
        }

        if (lot != null && lot.textNodes().size() > 1) {
            final List<TextNode> nodes = lot.textNodes();

            return new ParsedTenderLot()
                    .setPositionOnPage("1")
                    .setContractNumber(nodes.get(0).text())
                    .setTitle(nodes.get(1).text())
                    .setBidsCount(parseIinTD("Gautų pasiūlymų skaičius:", document))
                    .setAwardDecisionDate(parseIinTD("Sutarties sudarymo data:", document))
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .addBidder(new ParsedBody()
                                    .setName(parseIinTD("Ūkio subjektas, su kuriuo buvo sudaryta sutar" +
                                            "tis:", document))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(parseIinTD("Ūkio subjektas, su kuriuo buvo sudaryta " +
                                                    "sutartis:", document))
                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                            .setScope(BodyIdentifier.Scope.LT))
                                    .setAddress(new ParsedAddress()
                                            .setRawAddress(parseLotAddress(document))
                                    )
                            )
                            .setIsSubcontracted(parseIinTD("Ar tikėtina, kad sutartis bus perduota vykdyti" +
                                    " subrangovams", document))
                            .setPrice(parseLotFinalPrice(document))
                    );
        } else {
            return null;
        }
    }

    /**
     * Parse lot final price.
     *
     * @param document document to parse from
     *
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseLotFinalPrice(final Element document) {
        final String finalPrice = parseIinTD("Bendra galutinė sutarties vertė:", document);
        if (finalPrice == null || finalPrice.isEmpty()) {
            return null;
        }

        boolean isWithWat = finalPrice.contains("Be PVM");

        final String[] finalPriceParts = finalPrice.split("Valiuta");

        if (finalPrice.length() > 1) {
            if (isWithWat) {
                return new ParsedPrice()
                        .setAmountWithVat(finalPriceParts[0])
                        .setCurrency(finalPriceParts[1]);
            } else {
                return new ParsedPrice()
                        .setNetAmount(finalPriceParts[0])
                        .setCurrency(finalPriceParts[1]);
            }
        } else {
            return null;
        }
    }

    /**
     * Parse lot address.
     *
     * @param document document to parse from.
     *
     * @return String or null;
     */
    private static String parseLotAddress(final Element document) {
        final Elements addresses = document.select("tr tr:contains(Adresas, telefonas, faksas:)");

        if (addresses.size() > 1) {
            return addresses.get(1).text();
        } else {
            return null;
        }
    }

    /**
     * Parse supply type.
     *
     * @param document document to parse from.
     *
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
     *
     * @return String or null;
     */
    private static String parseBuyerAssignedId(final Element document) {
        String buyerAssignedId = parseOwnText("td b:has(i:containsOwn(Pirkimo numeris:))", document);

        if (buyerAssignedId == null) {
            buyerAssignedId = parseOwnText("i:containsOwn(numeris)", document);
        }

        return buyerAssignedId;
    }

    /**
     * Select "i" in "td".
     *
     * @param selector selector
     * @param element element to parse from
     *
     * @return String or null
     */
    private static String parseIinTD(final String selector, final Element element) {
        final String parsedText = parseOwnText("td td:has(i:containsOwn(" + selector + "))", element);

        if (parsedText != null) {
            return parsedText;
        } else {
            return parseOwnText("td:has(i:containsOwn(" + selector + "))", element);
        }
    }

    /**
     * Select "i" in "td".
     *
     * @param selectors selector
     * @param element element to parse from
     *
     * @return String or null
     */
    private static String parseIinTD(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = parseIinTD(selector, element);

            if (result != null && !result.isEmpty()) {
                return result;
            }
        }

        return null;
    }
}
