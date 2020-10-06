package eu.datlab.worker.cl.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.UnitType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

/**
 * Parser for Chile.
 */
public final class MPTenderParser extends BaseDatlabTenderParser {
    private static final Pattern BIDDER_COMMON_REGEX = Pattern.compile("^(?<id>\\d{1,}\\.\\d{3}\\.\\d{3}-[a-zA-Z0-9])( (?<name>.+))?");
    private static final Pattern BIDDER_TMI_REGEX = Pattern.compile("^(?<id>TMI \\d+ NK)( (?<name>.+))?");
    private static final Pattern BIDDER_DUMB_REGEX = Pattern.compile("^(?<id>[^ ]+)( (?<name>.+))?");

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "CL";
    }

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        final Document doc = Jsoup.parse(raw.getSourceData());
        final Object rawAdjudicacion = raw.getMetaData().get("adjudicacion");
        Document adjudicacion = null;
        if (rawAdjudicacion != null) {
            adjudicacion = Jsoup.parse((String) ((HashMap) rawAdjudicacion).get("body"));
        }

        final Object rawHistorial = raw.getMetaData().get("historial");
        Document historial = null;
        if (rawHistorial != null) {
            historial = Jsoup.parse((String) ((HashMap) rawHistorial).get("body"));
        }

        final ParsedTender parsedTender = new ParsedTender();

        parsedTender
            .setTitle(getById("lblFicha1Nombre", doc))
            .setCountry("CL")
            .setDescription(getById("lblFicha1Descripcion", doc))
            .setNationalProcedureType(getById("lblFicha1Convocatoria", doc))
            .setBidDeadline(getById("lblFicha3Cierre", doc))
            .addPublication(new ParsedPublication()
                .setSourceId(getById("lblNumLicitacion", doc))
                .setSource(PublicationSources.CL_MERCADO)
                .setHumanReadableUrl(raw.getSourceUrl().toString())
                .setPublicationDate(getById("lblTitlePorcDateDesc", adjudicacion))
                .setSourceFormType("Adjudicada")
                .setIsIncluded(true)
            )
            .addPublication(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.CL_MERCADO)
                .setSourceFormType("Fecha de Publicación")
                .setPublicationDate(getById("lblFicha3Publicacion", doc))
            )
            .addPublication(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.CL_MERCADO)
                .setSourceFormType("Fecha de Cierre")
                .setPublicationDate(getById("lblFicha3Cierre", doc))
            )
            .addBuyer(new ParsedBody()
                .setName(getById("lblFicha2Razon", doc))
                .addMainActivity(getById("lblFicha2Unidad", doc))
                .addBodyId(new BodyIdentifier()
                    .setId(getById("lblFicha2RUT", doc))
                    .setScope(BodyIdentifier.Scope.CL)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                )
                .setAddress(new ParsedAddress()
                    .setStreet(getById("lblFicha2Direccion", doc))
                    .setCity(getById("lblFicha2Comuna", doc))
                    .setState(getById("lblFicha2Region", doc))
                )
                .setContactName(getById("lblFicha7NombreResponsableContrato", doc))
                .setEmail(getById("lblFicha7EmailResponsableContrato", doc))
                .setPhone(getById("lblFicha7TelefonoResponsableContrato", doc))
            )
            .setEnquiryDeadline(getById("lblFicha3Fin", doc))
            .addCpv(new ParsedCPV()
                .setCode(getById("grvProducto_ctl02_lblCategoria", doc))
                .setIsMain(Boolean.TRUE.toString())
            )
            .setAwardDecisionDate(getById("lblFicha3Adjudicacion", doc))
            .setAwardCriteria(parseCriteria(doc))
            .setEligibilityCriteria(JsoupUtils.selectText("div.tabla_ficha_00:has(span#lblNatural)", doc))
            .addFunding(new ParsedFunding().setSource(getById("lblFicha7Financiamiento", doc)))
            .setSize(getById("lblFicha1Tipo", doc))
            .setDeposits(getById("grvGarantias_ctl02_lblFicha8Monto", doc) + " " + getById("grvGarantias_ctl02_lblFicha8TipoMoneda", doc))
            .addLots(parseLots(rawAdjudicacion));

        if (adjudicacion != null) {
            parsedTender
                .setFinalPrice(new ParsedPrice()
                    .setNetAmount(getById("lblAmountShow", adjudicacion))
                    .setCurrency(getById("lblCurrencyShow", adjudicacion))
                )
                .setEstimatedPrice(new ParsedPrice()
                    .setNetAmount(getById("lblEstimatedAmountShow", adjudicacion))
                    .setCurrency(getById("lblCurrencyShow", adjudicacion))
                );
        }

        return Collections.singletonList(parsedTender);
    }

    /**
     * Parse criteria.
     *
     * @param doc doc to parse from
     * @return List<ParsedAwardCriterion>
     */
    private List<ParsedAwardCriterion> parseCriteria(final Document doc) {
        final List<ParsedAwardCriterion> parsedAwardCriteria = new ArrayList<>();

        for (Element rawCriterion : JsoupUtils.select("tr[class=estiloSeparador]", doc)) {
            parsedAwardCriteria.add(new ParsedAwardCriterion()
                    .setName(getByRegexId(".*_lblNombreCriterio", rawCriterion))
                    .setDescription(getByRegexId(".*_lblObservaciones", rawCriterion))
                    .setWeight(getByRegexId(".*_lblPonderacion", rawCriterion))
            );
        }

        return parsedAwardCriteria;
    }

    /**
     * Parse lots.
     *
     * @param rawAdjudicacion object to parse from
     * @return List<ParsedTenderLot>
     */
    private List<ParsedTenderLot> parseLots(final Object rawAdjudicacion) {
        if (rawAdjudicacion == null) {
            return null;
        }

        @SuppressWarnings("unchecked") final HashMap<String, Object> adjudicacion = (HashMap<String, Object>) rawAdjudicacion;
        final Document rawLots = Jsoup.parse((String) adjudicacion.get("body"));

        final List<ParsedTenderLot> parsedTenderLots = new ArrayList<>();

        Integer onPageLotCounter = 1;
        for (Element lotHeader : JsoupUtils.select("table#rptBids_", rawLots)) {
            parsedTenderLots.add(new ParsedTenderLot()
                    .setTitle(getByRegexId("lblNameShow", rawLots))
                    .setLotId(getByRegexId(".*_ucAward_lblCodeonu", lotHeader))
                    .setPositionOnPage(onPageLotCounter.toString())
                    .setLotNumber(getByRegexId(".*_ucAward__lblNumber", lotHeader))
                    .addCpv(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(getByRegexId(".*_ucAward_lblCodeonu", lotHeader))
                    )
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(getById("lblEstimatedAmountShow", rawLots))
                            .setCurrency(getById("lblCurrencyShow", rawLots)))
                    .setBids(parseBids(JsoupUtils.select("tr.cssPRCGridViewHeader ~ tr", lotHeader.parent()), lotHeader)));
            onPageLotCounter++;
        }

        return parsedTenderLots;
    }

    /**
     * Parse bids.
     *
     * @param lotBids      parse from this
     * @param lotHeader      parse from this
     * @return List<ParsedBid>
     */
    private List<ParsedBid> parseBids(final Elements lotBids, final Element lotHeader) {
        if (lotBids == null) {
            return null;
        }

        final List<ParsedBid> parsedBids = new ArrayList<>();



        for (Element bid : lotBids) {
            final String finalPrice = getByRegexId(".*_lblTotalNetAward", bid);

            String unitPrice =  getByRegexId(".*_txtAwardedQuantity", bid);
            if (unitPrice.trim().equals("0")) {
                unitPrice = getByRegexId(".*_ucAward__LblRBICuantityNumber", lotHeader);
            }

            parsedBids.add(new ParsedBid()
                    .setPrice(finalPrice.equals("0") ? null : new ParsedPrice()
                            .setNetAmount(finalPrice)
                            .setCurrency(getByRegexId(".*_lblSymbol", bid))
                    )
                    .addUnitPrice(new ParsedUnitPrice()
                            .setNetAmount(getByRegexId(".*lblTotalNetPrice", bid))
                            .setCurrency(getByRegexId(".*_lblSymbol", bid))
                            .setUnitNumber(unitPrice)
                            .setUnitType(UnitType.PIECES.name())
                    )
                    .setIsWinning(getByRegexId(".*_lblIsSelected", bid))
                    .setBidders(parseBidders(bid))
            );
        }

        return parsedBids.isEmpty() ? null : parsedBids;
    }

    /**
     * Parse bidders.
     *
     * @param bid          bid to parse from
     * @return List<ParsedBody>
     */
    private List<ParsedBody> parseBidders(final Element bid) {
        if (bid == null) {
            return null;
        }

        final List<ParsedBody> result = new ArrayList<>();
        final List<String> bidders = JsoupUtils.select("a", bid).stream()
            .map(Element::text).filter(n -> n != null && !n.isEmpty()).collect(Collectors.toList());

        for (String bidder : bidders) {
            Matcher m = getMatcher(bidder, BIDDER_COMMON_REGEX, BIDDER_TMI_REGEX, BIDDER_DUMB_REGEX);
            if (m.find()) {
                result.add(new ParsedBody()
                    .setName(m.group("name"))
                    .addBodyId(new BodyIdentifier()
                        .setId(m.group("id"))
                        .setScope(BodyIdentifier.Scope.CL)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    ));
            } else {
                logger.error("Unexpected format of the bidder '{}'", bidder);
                throw new UnrecoverableException("Unexpected bidder format");
            }
        }

        return result;
    }

    /**
     * Returns matcher for first matching pattern. If no pattern matches input, returns last matcher.
     *
     * @param input
     *      input to be matched
     * @param patterns
     *      list of patterns to be tested
     * @return matcher
     */
    private static Matcher getMatcher(final String input, final Pattern... patterns) {
        Matcher matcher = null;
        for (Pattern p : patterns) {
            matcher = p.matcher(input);
            if (matcher.find()) {
                break;
            }
        }

        return matcher.reset();
    }

    /**
     * Parse by Id.
     *
     * @param selector selector
     * @param element  element
     * @return String
     */
    private static String getById(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        return JsoupUtils.selectText("*#" + selector, element);
    }

    /**
     * Parse by regex Id.
     *
     * @param selector selector
     * @param element  element
     * @return String
     */
    private static String getByRegexId(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        return JsoupUtils.selectText("*[id~=" + selector + "]", element);
    }

    /**
     * Generate xpath of element.
     *
     * @param element element
     * @return String
     */
    private static String generateElementXpath(final Element element) {
        if (element == null) {
            return null;
        }

        final StringBuilder path = new StringBuilder();
        final Elements parents = element.parents();

        for (int j = parents.size() - 1; j >= 0; j--) {
            path.append("/");
            path.append(parents.get(j).tagName());
            if (getSameElementSiblingIndex(parents.get(j)) != 0) {
                path.append("[");
                path.append(getSameElementSiblingIndex(parents.get(j)));
                path.append("]");
            }
        }

        path.append("/").append(element.tagName());

        return path.toString();
    }

    /**
     * Get sibling index of element.
     *
     * @param element element
     * @return String
     */
    private static int getSameElementSiblingIndex(final Element element) {
        final Elements siblings = element.parent().select("> " + element.tagName());

        if (siblings.size() == 1) {
            return 0;
        } else {
            for (int siblingIndex = 0; siblingIndex < siblings.size(); siblingIndex++) {
                if (siblings.get(siblingIndex).siblingIndex() == element.siblingIndex()) {
                    return siblingIndex + 1;
                }
            }

            throw new UnrecoverableException("This method doesn't function properly, it should never end up here.");
        }
    }

    @Override
    protected String getVersion() {
        return "1.0";
    }

    /**
     * Decode and extract input into string.
     *
     * @param input input
     * @return String
     */
    private String extract(final byte[] input) {
        try {
            if (input == null) {
                return null;
            }

            final ByteArrayInputStream bais = new ByteArrayInputStream(input);
            final GZIPInputStream gzis = new GZIPInputStream(bais);
            final InputStreamReader reader = new InputStreamReader(gzis);
            final BufferedReader in = new BufferedReader(reader);

            final StringBuilder result = new StringBuilder();
            String line;
            while ((line = in.readLine()) != null) {
                result.append(line);
            }

            return result.toString();
        } catch (IOException e) {
            logger.error("Cannot create output stream {}", e);
            throw new UnrecoverableException("Cannot create output stream", e);
        }
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
