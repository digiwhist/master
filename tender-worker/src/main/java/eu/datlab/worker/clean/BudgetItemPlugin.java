package eu.datlab.worker.clean;

import eu.datlab.dataaccess.dto.clean.CleanBudgetItem;
import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.worker.clean.plugin.BaseNumberPlugin;
import eu.dl.worker.clean.utils.BodyUtils;
import eu.dl.worker.clean.utils.CodeTableUtils;
import eu.dl.worker.clean.utils.NumberUtils;
import eu.dl.worker.clean.utils.PriceUtils;
import eu.dl.worker.clean.utils.StringUtils;
import eu.dl.worker.clean.utils.URLSchemeType;

import java.text.NumberFormat;
import java.util.List;
import java.util.Map;

/**
 * Plugin for cleaning buget items.
 *
 * @author Tomas Mrazek
 */
public class BudgetItemPlugin extends BaseNumberPlugin<ParsedBudgetItem, CleanBudgetItem> {

    private final Map<String, Map<Enum, List<String>>> mappings;

    /**
     * Constat for key in mappings that holds buyerType mapping.
     */
    public static final String BUYER_TYPE_MAPPING = "buyerTypeMapping";
    /**
     * Constat for key in mappings that holds buyerActivityType mapping.
     */
    public static final String BUYER_ACTIVITY_TYPE_MAPPING = "buyerActivityTypeMapping";
    /**
     * Constat for key in mappings that holds country mapping.
     */
    public static final String COUNTRY_MAPPING = "countryMapping";
    /**
     * Constat for key in mappings that holds budgetItemReportType mapping.
     */
    public static final String BUDGET_ITEM_REPORT_TYPE_MAPPING = "budgetItemReportTypeMapping";

    /**
     * BudgetItemPlugin with number format and report type mapping initialization .
     *
     * @param format
     *        number format
     * @param mappings
     *        mappings for cleaning process. Expects keys (buyerTypeMapping, buyerActivityTypeMapping, countryMapping,
     *        budgetItemReportTypeMapping) that hodl appropriate mappings.
     */
    public BudgetItemPlugin(final NumberFormat format, final Map<String, Map<Enum, List<String>>> mappings) {
        super(format);
        this.mappings = mappings;
    }

    /**
     * BudgetItemPlugin with list of number formats and report type mapping initialization .
     *
     * @param formats
     *        list of number formats
     * @param mappings
     *        mappings for cleaning process. Expects keys (buyerTypeMapping, buyerActivityTypeMapping, countryMapping,
     *        budgetItemReportTypeMapping) that hodl appropriate mappings.
     */
    public BudgetItemPlugin(final List<NumberFormat> formats, final Map<String, Map<Enum, List<String>>> mappings) {
        super(formats);
        this.mappings = mappings;
    }

    /**
     * Cleans budget item.
     *
     * @param parsedBudget
     *         parsed budget with source data
     * @param cleanBudget
     *         budget with clean data
     *
     * @return budget with cleaned data
     */
    @Override
    public final CleanBudgetItem clean(final ParsedBudgetItem parsedBudget, final CleanBudgetItem cleanBudget) {
        
        cleanBudget
            .setBody(BodyUtils.cleanBody(parsedBudget.getBody(), mappings.get(BUYER_TYPE_MAPPING),
                mappings.get(BUYER_ACTIVITY_TYPE_MAPPING), mappings.get(COUNTRY_MAPPING)))            
            .setLevel1Name(StringUtils.cleanShortString(parsedBudget.getLevel1Name()))
            .setLevel1Code(StringUtils.cleanShortString(parsedBudget.getLevel1Code()))
            .setLevel2Name(StringUtils.cleanShortString(parsedBudget.getLevel2Name()))
            .setLevel2Code(StringUtils.cleanShortString(parsedBudget.getLevel2Code()))            
            .setLevel3Name(StringUtils.cleanShortString(parsedBudget.getLevel3Name()))
            .setLevel3Code(StringUtils.cleanShortString(parsedBudget.getLevel3Code()))
            .setCurrency(PriceUtils.cleanPriceCurrency(parsedBudget.getCurrency()))            
            .setPlannedValue(NumberUtils.cleanBigDecimal(parsedBudget.getPlannedValue(), formats))
            .setReport((BudgetItemReportType) CodeTableUtils.mapValue(parsedBudget.getReport(),
                mappings.get(BUDGET_ITEM_REPORT_TYPE_MAPPING)))
            .setSource(StringUtils.cleanURL(parsedBudget.getSource(), URLSchemeType.HTTP))
            .setValue(NumberUtils.cleanBigDecimal(parsedBudget.getValue(), formats))
            .setYear(NumberUtils.cleanInteger(parsedBudget.getYear(), formats))
            .setReportSubType(StringUtils.cleanShortString(parsedBudget.getReportSubType()));

        return cleanBudget;
    }
}
