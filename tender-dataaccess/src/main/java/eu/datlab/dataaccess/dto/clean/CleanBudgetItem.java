package eu.datlab.dataaccess.dto.clean;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.clean.CleanStorableDTO;
import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.math.BigDecimal;
import java.net.URL;
import java.util.Currency;

/**
 * Clean budget item. Contains full info about budget item.
 */
public class CleanBudgetItem extends CleanStorableDTO implements Cleanable {
    /**
     * Report type.
     */
    private BudgetItemReportType report;
    /**
     * Year to which item is relevant.
     */
    private Integer year;
    /**
     * Name of the most detailed item.
     */
    private String level3Name;
    /**
     * Code of the most detailed item.
     */
    private String level3Code;
    /**
     * Name of the higher level item.
     */
    private String level2Name;
    /**
     * Code of the higher level item.
     */
    private String level2Code;
    /**
     * Value of the item.
     */
    private BigDecimal value;
    /**
     * Planned value.
     */
    private BigDecimal plannedValue;
    /**
     * Currency.
     */
    private Currency currency;
    /**
     * URL where the item was obtained.
     */
    private URL source;
    /**
     * Budget body.
     */
    private CleanBody body;
    
    private String hash;

    private String groupId;

    private String matchedBy;
    /**
     * Name of the budget chapter.
     */
    private String level1Name;
    /**
     * Code of the budget chapter.
     */
    private String level1Code;
    /**
     * Country ISO code.
     */
    private CountryCode country;

    /**
     * Report sub type.
     */
    private String reportSubType;

    /**
     * Gets the report.
     *
     * @return the report
     */
    public final BudgetItemReportType getReport() {
        return report;
    }

    /**
     * Sets the report.
     *
     * @param newReport
     *            the new report
     * @return the clean budget item
     */
    public final CleanBudgetItem setReport(final BudgetItemReportType newReport) {
        this.report = newReport;
        return this;
    }

    /**
     * Gets the year.
     *
     * @return the year
     */
    public final Integer getYear() {
        return year;
    }

    /**
     * Sets the year.
     *
     * @param newYear
     *            the new year
     * @return the clean budget item
     */
    public final CleanBudgetItem setYear(final Integer newYear) {
        this.year = newYear;
        return this;
    }

    /**
     * Gets the level 3 name.
     *
     * @return the level 3 name
     */
    public final String getLevel3Name() {
        return level3Name;
    }

    /**
     * Sets the level 3 name.
     *
     * @param name
     *            the level 3 name
     * @return the clean budget item
     */
    public final CleanBudgetItem setLevel3Name(final String name) {
        this.level3Name = name;
        return this;
    }

    /**
     * Gets the level 3 code.
     *
     * @return the level 3 code
     */
    public final String getLevel3Code() {
        return level3Code;
    }

    /**
     * Sets the level 3 code.
     *
     * @param code
     *            the level 3 code
     * @return the clean budget item
     */
    public final CleanBudgetItem setLevel3Code(final String code) {
        this.level3Code = code;
        return this;
    }

    /**
     * Gets the level 2 name.
     *
     * @return the level 2 name
     */
    public final String getLevel2Name() {
        return level2Name;
    }

    /**
     * Sets the level 2 name.
     *
     * @param name
     *            the level 2 name
     * @return the clean budget item
     */
    public final CleanBudgetItem setLevel2Name(final String name) {
        this.level2Name = name;
        return this;
    }

    /**
     * Gets the level 2 code.
     *
     * @return the level 2 code
     */
    public final String getLevel2Code() {
        return level2Code;
    }

    /**
     * Sets the level 2 code.
     *
     * @param code
     *            the level 2 code
     * @return the clean budget item
     */
    public final CleanBudgetItem setLevel2Code(final String code) {
        this.level2Code = code;
        return this;
    }

    /**
     * Gets the value.
     *
     * @return the value
     */
    public final BigDecimal getValue() {
        return value;
    }

    /**
     * Sets the value.
     *
     * @param newValue
     *            the new value
     * @return the clean budget item
     */
    public final CleanBudgetItem setValue(final BigDecimal newValue) {
        this.value = newValue;
        return this;
    }

    /**
     * Gets the planned value.
     *
     * @return the planned value
     */
    public final BigDecimal getPlannedValue() {
        return plannedValue;
    }

    /**
     * Sets the planned value.
     *
     * @param newPlannedValue
     *            the new planned value
     * @return the clean budget item
     */
    public final CleanBudgetItem setPlannedValue(final BigDecimal newPlannedValue) {
        this.plannedValue = newPlannedValue;
        return this;
    }

    /**
     * Gets the currency.
     *
     * @return the currency
     */
    public final Currency getCurrency() {
        return currency;
    }

    /**
     * Sets the currency.
     *
     * @param newCurrency
     *            the new currency
     * @return the clean budget item
     */
    public final CleanBudgetItem setCurrency(final Currency newCurrency) {
        this.currency = newCurrency;
        return this;
    }

    /**
     * Gets the source.
     *
     * @return the source
     */
    public final URL getSource() {
        return source;
    }

    /**
     * Sets the source.
     *
     * @param newSource
     *            the new source
     * @return the clean budget item
     */
    public final CleanBudgetItem setSource(final URL newSource) {
        this.source = newSource;
        return this;
    }

    /**
     * Gets the body.
     *
     * @return the body
     */
    public final CleanBody getBody() {
        return body;
    }

    /**
     * Sets the body.
     *
     * @param newBody
     *            the new body
     * @return the clean budget item
     */
    public final CleanBudgetItem setBody(final CleanBody newBody) {
        this.body = newBody;
        return this;
    }

    /**
     * Gets the group id.
     *
     * @return the group id
     */
    public final String getGroupId() {
        return groupId;

    }

    /**
     * Sets the group id.
     *
     * @param groupId
     *            the group id
     * @return the clean budget item
     */
    public final CleanBudgetItem setGroupId(final String groupId) {
        this.groupId = groupId;
        return this;
    }

    /**
     * Gets the matched by.
     *
     * @return the matched by
     */
    public final String getMatchedBy() {
        return matchedBy;
    }

    /**
     * Sets the matched by.
     *
     * @param matchedBy
     *            the matched by
     * @return the clean budget item
     */
    public final CleanBudgetItem setMatchedBy(final String matchedBy) {
        this.matchedBy = matchedBy;
        return this;
    }

    /**
     * Gets the hash.
     *
     * @return the hash
     */
    public final String getHash() {
        return hash;
    }

    /**
     * Sets the hash.
     *
     * @param hash
     *            the hash
     * @return the clean budget item
     */
    public final CleanBudgetItem setHash(final String hash) {
        this.hash = hash;
        return this;
    }
    
    /**
     * @return level 1 name
     */
    public final String getLevel1Name() {
        return level1Name;
    }

    /**
     * @param name
     *      level 1 name to set
     * @return this instance for chaining
     */
    public final CleanBudgetItem setLevel1Name(final String name) {
        this.level1Name = name;
        return this;
    }

    /**
     * @return level 1 code
     */
    public final String getLevel1Code() {
        return level1Code;
    }

    /**
     * @param code
     *      level 1 code to set
     * @return this instance for chaining
     */
    public final CleanBudgetItem setLevel1Code(final String code) {
        this.level1Code = code;
        return this;
    }

    /**
     * @return country ISO code
     */
    public final CountryCode getCountry() {
        return country;
    }

    /**
     * @param country
     *      country ISO code to set
     * @return this instance for chaining
     */
    public final CleanBudgetItem setCountry(final CountryCode country) {
        this.country = country;
        return this;
    }

    @Override    
    @JsonIgnore
    public final CleanBudgetItem getValid() {
        setBody(ClassUtils.removeNonsenses(body));

        return ValidationUtils.getValid(this, body, country, currency, groupId, hash, level1Code, level1Name,
            level2Code, level2Name, level3Code, level3Name, plannedValue, report, source, value, year);
    }

    /**
     * @return report sub type
     */
    public final String getReportSubType() {
        return reportSubType;
    }

    /**
     *
     * @param reportSubType
     *      report sub type to be set
     * @return this instance for chaining
     */
    public final CleanBudgetItem setReportSubType(final String reportSubType) {
        this.reportSubType = reportSubType;
        return this;
    }
}
