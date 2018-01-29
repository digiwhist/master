package eu.digiwhist.worker.pl.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;

/**
 * Class provides set of functions for parsing typical xml fragments like address, subject etc.
 *
 * @author Tomas Mrazek
 */
public final class UZPTenderParserUtils {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private UZPTenderParserUtils() {
        throw new AssertionError();
    }

    /**
     * Parses award criteria.
     *
     * @param context
     *      context
     * @return list of award criteria
     */
    public static List<ParsedAwardCriterion> parseAwardCriteria(final Element context) {
        final List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        final Element priceCriterion = JsoupUtils.selectFirst(":root > kryt_cena", context);
        int otherCriteriaNumberingStart = 1;
        if (priceCriterion != null) {
            awardCriteria.add(parseAwardCriterion(priceCriterion).setIsPriceRelated(Boolean.TRUE.toString()));
            otherCriteriaNumberingStart = 2;
        }

        final Elements otherCriteria =
            JsoupUtils.selectNumberedElements((number) -> ":root > kryt_".concat(number.toString()), context,
                otherCriteriaNumberingStart);
        for (Element criterion : otherCriteria) {
            awardCriteria.add(parseAwardCriterion(criterion));
        }

        return (awardCriteria.isEmpty() ? null : awardCriteria);
    }

    /**
     * Parses award criterion.
     *
     * @param criterionNode
     *      criterion node
     * @return parsed award criterion
     */
    public static ParsedAwardCriterion parseAwardCriterion(final Element criterionNode) {
        //in case that price related criterion is defined this one is listed first and weight node has name kryt_1p
        final String weightNodeSelector =
            (criterionNode.tagName().equalsIgnoreCase("kryt_cena") ? "kryt_1p" : criterionNode.tagName().concat("p"));
        final Element weightNode = JsoupUtils.selectFirst(weightNodeSelector, criterionNode.parent());

        return new ParsedAwardCriterion()
            .setName(criterionNode.text())
            .setWeight((weightNode == null ? null : weightNode.text()));
    }

    /**
     * Parses CPVs.
     *
     * @param context
     *      context
     * @return list of prased CPVs
     */
    public static List<ParsedCPV> parseCPVs(final Element context) {
        final Elements cpvNodes =
            JsoupUtils.selectNumberedElements((number) -> String.format(":root cpv%1$dc", number), context);

        if (cpvNodes.isEmpty()) {
            return null;
        }

        final List<ParsedCPV> cpvs = new ArrayList<>();
        cpvNodes.stream().forEach(node -> {
            cpvs.add(new ParsedCPV()
                .setIsMain(Boolean.FALSE.toString())
                .setCode(node.text()));
        });

        // set main cpv code
        cpvs.get(0).setIsMain(Boolean.TRUE.toString());

        return cpvs;
    }

    /**
     * Check whether option is enabled. It depends on element text, in case that is equal to 'A' option is enabled.
     * If is equal to 'N' or any string is disabled.
     *
     * @param selector
     *      selector
     * @param context
     *      context
     * @return the decision of whether option is enabled
     */
    public static Boolean isEnabled(final String selector, final Element context) {
        final Element element = JsoupUtils.selectFirst(selector, context);
        if (element != null) {
            return element.text().trim().equalsIgnoreCase("A");
        }

        return false;
    }


    /**
     * Parses common data and returns initialized parsed tender.
     *
     * @param document
     *         parsed xml document
     * @param machineReadableUrl
     *          machine readable url for included publication
     * @return parsed tender
     */
    public static ParsedTender parseCommonFormData(final Document document, final String machineReadableUrl) {
        final ParsedTender parsedTender = new ParsedTender();

        String nationalProcedureType = JsoupUtils.selectText("kod_trybu", document);

        parsedTender
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setSourceId(JsoupUtils.selectText("pozycja", document))
                .setPublicationDate(JsoupUtils.selectText("data_publikacji", document))
                .setSourceFormType(JsoupUtils.selectText("ogloszenie", document))
                .setMachineReadableUrl(machineReadableUrl))
            .addBuyer(parseBuyer(document))
            .setHasLots(parseHasLots(document))
            .setTitle(JsoupUtils.selectText("nazwa_zamowienia, nazwa_konkursu", document))
            .setDescription(JsoupUtils.selectText("przedmiot_zam", document))
            .setSupplyType(JsoupUtils.selectText("rodz_zam", document))
            .setProcedureType(nationalProcedureType)
            .setNationalProcedureType(nationalProcedureType)
            .setCpvs(UZPTenderParserUtils.parseCPVs(document));

        return parsedTender;
    }

    /**
     * Parse if tender has lots.
     *
     * @param document document to parse from
     * @return String or null
     */
    public static String parseHasLots(final Document document) {
        final String hasLots = JsoupUtils.selectText("czy_czesci", document);

        if (hasLots == null) {
            return null;
        } else if (hasLots.contains("Y")) {
            return Boolean.TRUE.toString();
        } else if (hasLots.contains("N")) {
            return Boolean.FALSE.toString();
        } else {
            return null;
        }
    }

    /**
     * Parses buyer from the given document.
     *
     * @param document
     *          parsed document
     * @return parsed buyer
     */
    public static ParsedBody parseBuyer(final Document document) {
        String street = JsoupUtils.selectText("ulica", document);
        String number = JsoupUtils.selectText("nr_domu", document);
        if (street != null && number != null && !street.trim().endsWith(number.trim())) {
            street = street + " " + number;
        } else if (street == null && number != null) {
            street = number;
        }
        
        ParsedBody buyer = new ParsedBody()
            .setName(JsoupUtils.selectText("nazwa", document))
            .setAddress(new ParsedAddress()
                .setStreet(street)
                .setCity(JsoupUtils.selectText("miejscowosc", document))
                .setPostcode(JsoupUtils.selectText("kod_poczt", document))
                .setState(JsoupUtils.selectText("wojewodztwo", document))
                .setUrl(JsoupUtils.selectText("internet", document)))
            .setPhone(JsoupUtils.selectText("tel", document))
            .setEmail(JsoupUtils.selectText("e_mail", document))
            .setBuyerType(JsoupUtils.selectText("rodzaj_zam_inny, rodzaj_zam", document));

        String bodyId = JsoupUtils.selectText("regon", document);
        if (bodyId != null) {
            buyer.addBodyId(new BodyIdentifier()
                .setId(bodyId)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                .setScope(BodyIdentifier.Scope.PL));
        }

        return buyer;
    }

}
