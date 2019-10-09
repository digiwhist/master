package eu.datlab.worker.es.parsed;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Parser handler used for tender html detail parsing.
 *
 * @author Tomas Mrazek
 */
public final class PCETenderHtmlDetailHandler {

    private static final Pattern CPV_REGEX = Pattern.compile("\\d{8}(\\-\\d+)?");

    /**
     * Suppress default constructor for noninstantiability.
     */
    private PCETenderHtmlDetailHandler() {
    }

    /**
     * Parses tender HTML detail.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     * @param formType
     *         tender type
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document, final String formType) {
        parsedTender
            .addPublication(new ParsedPublication()
                .setHumanReadableUrl(JsoupUtils.selectAttribute(getIdSelector("URLgenera"), "href", document))
                .setSourceFormType(formType)
                .setSource(PublicationSources.ES_PCE)
                .setIsIncluded(true))
            .addPublications(parseRelatedPublications(document))
            .setTitle(JsoupUtils.selectText(getIdSelector("text_ObjetoContrato"), document))
            .setEstimatedPrice(parsePrice(getIdSelector("text_ValorContrato"), document))
            .setSupplyType(JsoupUtils.selectText(getIdSelector("text_TipoContrato"), document))
            .addLot(new ParsedTenderLot()
                .setStatus(JsoupUtils.selectText(getIdSelector("text_Resultado"), document))
                .setBidsCount(JsoupUtils.selectText(getIdSelector("text_NumeroLicitadores"), document))
                .addBid(new ParsedBid()
                    .setPrice(parsePrice(getIdSelector("text_ImporteAdjudicacion"), document))
                    .addBidder(new ParsedBody()
                        .setName(JsoupUtils.selectText(getIdSelector("text_Adjudicatario"), document)))))
            .addBuyer(new ParsedBody()
                .setName(JsoupUtils.selectText(getIdSelector("text_OC_con"), document)))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(JsoupUtils.selectText(getIdSelector("text_LugarEjecucion"), document)))
            .setProcedureType(JsoupUtils.selectText("text_Procedimiento", document));

        String cpv = JsoupUtils.selectText(getIdSelector("text_CPV"), document);
        if (cpv != null) {
            Matcher m = CPV_REGEX.matcher(cpv);
            while (m.find()) {
                parsedTender.addCpv(new ParsedCPV().setCode(m.group()));
            }
        }

        Element publicationsTable = JsoupUtils.selectFirst("table#myTablaDetallePublicacionesPlatAgreVISUOE > tbody", document);

        parsedTender
            .setAwardDecisionDate(JsoupUtils.selectText("tr:has(td:eq(1):contains(Adjudicación)):not(:has(td:eq(2):contains(Xml)))" +
                " > td:eq(0)", publicationsTable))
            .setContractSignatureDate(JsoupUtils.selectText("tr:has(td:eq(1):contains(Formalización)):not(:has(td:eq(2):contains(Xml)))" +
                " > td:eq(0)", publicationsTable));

        return parsedTender;

    }

    /**
     * @param id element id
     * @return element id selector
     */
    private static String getIdSelector(final String id) {
        return "*[id=viewns_Z7_AVEQAI930OBRD02JPMTPG21006_:form1:" + id + "]";
    }

    /**
     * Parse related publications form document.
     *
     * @param document document to parse from
     * @return List or null
     */
    private static List<ParsedPublication> parseRelatedPublications(final Document document) {
        final List<ParsedPublication> relatedPublications = new ArrayList<>();
        final Elements relatedPublicationsUrls =
                select("table[id=myTablaDetalleVISUOE] > tbody > tr a:containsOwn(Xml)", document);

        if (relatedPublicationsUrls != null && !relatedPublicationsUrls.isEmpty()) {
            for (Element relatedPublicationUrl : relatedPublicationsUrls) {
                relatedPublications.add(new ParsedPublication()
                        .setMachineReadableUrl(selectAttribute("href", relatedPublicationUrl))
                        .setSource(PublicationSources.ES_PCE)
                        .setIsIncluded(false));
            }
        }

        // Enlace a la licitación link except for the case when the link leads to contrataciondelestado.es domain
        String link = JsoupUtils.selectText("span[id=viewns_Z7_AVEQAI930OBRD02JPMTPG21006_:form1:text_EnlaceLicPLACE]", document);
        if (link != null && !link.startsWith("https://contrataciondelestado.es")) {
            relatedPublications.add(new ParsedPublication()
                .setHumanReadableUrl(link)
                .setSource(PublicationSources.ES_PCE)
                .setIsIncluded(false)
                .setSourceFormType(PublicationFormType.OTHER.name()));
        }


        return relatedPublications.isEmpty() ? null : relatedPublications;
    }

    /**
     * Parses price with currency.
     *
     * @param selector
     *      price selector
     * @param document
     *      pared document
     * @return parsed price
     */
    private static ParsedPrice parsePrice(final String selector, final Document document) {
        String currency = JsoupUtils.selectText(selector + " + span", document);
        if (currency != null && currency.equals("Euros")) {
            currency = "EUR";
        }

        return new ParsedPrice()
            .setNetAmount(JsoupUtils.selectText(selector, document))
            .setCurrency(currency);
    }
}
