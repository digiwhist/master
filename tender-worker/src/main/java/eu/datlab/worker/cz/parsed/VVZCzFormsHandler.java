package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for Czech forms.
 */
abstract class VVZCzFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZCzFormsHandler.class);

    // =================================
    // SECTION I
    // =================================

    // ---------------------------------
    // CZ SUBSECTION I.1)
    // --------------------------------

    /**
     * Parses buyer info.
     *
     * @param sectionI
     *         html for section I (with buyer info)
     *
     * @return parsed buyer info
     */
    static ParsedBody parseBuyer(final Element sectionI) {
        final Element buyerHtml = VVZTenderParserUtils.getFormSubsectionByName(sectionI, "I\\.1\\).*");
        return VVZTenderParser.parseBody(buyerHtml);
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.2) and II.5.3) and IV.2.2)
    // ---------------------------------

    /**
     * Parses main CPV.
     *
     * @param cpvSection
     *         html segment with CPV info
     *
     * @return main CPV
     */
    static ParsedCPV parseCpvCode(final Element cpvSection) {
        final String cpvCode = VVZTenderParserUtils.getFieldValue(cpvSection, ".*\\.CpvMain$");
        if (StringUtils.isNotEmpty(cpvCode)) {
            return new ParsedCPV().setIsMain(Boolean.TRUE.toString()).setCode(cpvCode);
        }
        return null;
    }

    /**
     * Parses address of implementation.
     *
     * @param sectionII
     *         section II html
     *
     * @return address of implementation
     */
    static ParsedAddress parseAddressOfImplementation(final Element sectionII) {
        return new ParsedAddress().setRawAddress(getFieldValue(sectionII, ".*MainSite"))
                .addNuts(getFieldValue(sectionII, ".*Nuts$"));
    }

    /**
     * Parses lot address of implementation.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot address of implementation
     */
    static ParsedAddress parseLotAddressOfImplementation(final Element lotHtml) {
        final String nuts = VVZTenderParserUtils.getFieldValue(lotHtml, ".*\\.Nuts$");
        if (StringUtils.isNotEmpty(nuts)) {
            return new ParsedAddress().addNuts(nuts);
        }
        return null;
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.3)
    // ---------------------------------

    /**
     * Parses tender description.
     *
     * @param sectionII
     *         section II html
     *
     * @return tender description
     */
    static String parseTenderDescription(final Element sectionII) {
        return VVZTenderParserUtils.getFieldValue(sectionII, ".*\\.ShortDesc$");
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.6)
    // ---------------------------------

    /**
     * Parses information about what the form includes (framework agreement, DPS, ...).
     *
     * @param sectionII
     *         section II html
     *
     * @return value of what the notice includes
     */
    static String parseNoticeInvolvesValues(final Element sectionII) {
        return VVZTenderParserUtils.getCheckedInputValue(sectionII, ".*NoticeInvolves$");
    }

    // ---------------------------------
    // CZ SUBSECTION II.2.1) and II.5.4)
    // ---------------------------------

    /**
     * Parses estimated price.
     *
     * @param priceSection
     *         html segment with price info
     *
     * @return estimated price
     */
    static ParsedPrice parseEstimatedPrice(final Element priceSection) {
        return new ParsedPrice().setCurrency("CZK")
                .setNetAmount(VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.EstimatedTotal\\.ValueFrom$"));
    }

    // ---------------------------------
    // CZ SUBSECTION III.2.1) and II.5.6)
    // ---------------------------------

    /**
     * Parses award criteria.
     *
     * @param root
     *         html segment with criteria info
     *
     * @return award criteria
     */
    static List<ParsedAwardCriterion> parseAwardCriteria(final Element root) {
        final String criteriaType = parseSelectionMethod(root);

        if (criteriaType == null) {
            return null;
        }

        List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();
        if (criteriaType.equalsIgnoreCase("AC_PRICE")) {
            parsedCriteria.add(new ParsedAwardCriterion().setIsPriceRelated(Boolean.TRUE.toString())
                    .setWeight(Integer.toString(100))
                    .setName("PRICE"));
        } else {
            final Boolean isPriceRelated = criteriaType.equalsIgnoreCase("AC_PROCUREMENT_DOC") ? true : null;
            for (Element criterion : root.select("div[model~=.*CriteriaList\\[\\d+\\]]")) {
                final ParsedAwardCriterion parsedCriterion = VVZPriorAndContractNoticeHandler.parseCriterion(criterion,
                        isPriceRelated);
                if (parsedCriterion != null) {
                    parsedCriteria.add(parsedCriterion);
                }
            }
        }
        return parsedCriteria;
    }

    /**
     * Parses selection method type (aka criteria type).
     *
     * @param root
     *         html segment with criteria info
     *
     * @return selection method
     */
    static String parseSelectionMethod(final Element root) {
        return VVZTenderParserUtils.getCheckedInputValue(root, ".*AwardCriteriaType$");
    }
}
