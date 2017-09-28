package eu.digiwhist.worker.pl.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.worker.utils.jsoup.JsoupUtils;

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
            awardCriteria.add(parseAwardCriterion(priceCriterion, true));
            otherCriteriaNumberingStart = 2;
        }

        final Elements otherCriteria =
            JsoupUtils.selectNumberedElements((number) -> ":root > kryt_".concat(number.toString()), context,
                otherCriteriaNumberingStart);
        for (Element criterion : otherCriteria) {
            awardCriteria.add(parseAwardCriterion(criterion, false));
        }

        return (awardCriteria.isEmpty() ? null : awardCriteria);
    }

    /**
     * Parses award criterion.
     *
     * @param criterionNode
     *      criterion node
     * @param isPriceRelated
     *      is price related
     * @return parsed award criterion
     */
    public static ParsedAwardCriterion parseAwardCriterion(final Element criterionNode,
        final Boolean isPriceRelated) {
        //in case that price related criterion is defined this one is listed first and weight node has name kryt_1p
        final String weightNodeSelector =
            (criterionNode.tagName().equalsIgnoreCase("kryt_cena") ? "kryt_1p" : criterionNode.tagName().concat("p"));
        final Element weightNode = JsoupUtils.selectFirst(weightNodeSelector, criterionNode.parent());

        return new ParsedAwardCriterion()
            .setName(criterionNode.text())
            .setWeight((weightNode == null ? null : weightNode.text()))
            .setIsPriceRelated(isPriceRelated.toString());
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
}
