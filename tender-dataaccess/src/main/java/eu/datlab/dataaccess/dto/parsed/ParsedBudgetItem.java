package eu.datlab.dataaccess.dto.parsed;

import eu.dl.dataaccess.dto.parsed.BaseParsedStorableDTO;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

/**
 * Parsed budget item. Contains full info about budget item.
 */
public class ParsedBudgetItem extends BaseParsedStorableDTO implements Parsable {
    /**
     * Report type.
     */
    private String report;
    /**
     * Year to which item is relevant.
     */
    private String year;
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
    private String value;
    /**
     * Planned value.
     */
    private String plannedValue;
    /**
     * ISO level3Code of the currency.
     */
    private String currency;
    /**
     * URL where the item was obtained.
     */
    private String source;
    /**
     * Budget body.
     */
    private ParsedBody body;
    /**
     * Name of the budget chapter.
     */
    private String level1Name;
    /**
     * Code of the budget chapter.
     */
    private String level1Code;
    /**
     * Country.
     */
    private String country;

    /**
     * Report sub type.
     */
    private String reportSubType;

    /**
     * Gets the report.
     *
     * @return the report
     */
    public final String getReport() {
        return report;
    }

    /**
     * Sets the report.
     *
     * @param newReport
     *            the new report
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setReport(final String newReport) {
        this.report = newReport;
        return this;
    }

    /**
     * Gets the year.
     *
     * @return the year
     */
    public final String getYear() {
        return year;
    }

    /**
     * Sets the year.
     *
     * @param newYear
     *            the new year
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setYear(final String newYear) {
        this.year = newYear;
        return this;
    }

    /**
     * Gets the level 3 name.
     *
     * @return the level3 name
     */
    public final String getLevel3Name() {
        return level3Name;
    }

    /**
     * Sets the level 3 name.
     *
     * @param name
     *            the new level 3 name
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setLevel3Name(final String name) {
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
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setLevel3Code(final String code) {
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
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setLevel2Name(final String name) {
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
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setLevel2Code(final String code) {
        this.level2Code = code;
        return this;
    }

    /**
     * Gets the value.
     *
     * @return the value
     */
    public final String getValue() {
        return value;
    }

    /**
     * Sets the value.
     *
     * @param newValue
     *            the new value
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setValue(final String newValue) {
        this.value = newValue;
        return this;
    }

    /**
     * Gets the planned value.
     *
     * @return the planned value
     */
    public final String getPlannedValue() {
        return plannedValue;
    }

    /**
     * Sets the planned value.
     *
     * @param newPlannedValue
     *            the new planned value
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setPlannedValue(final String newPlannedValue) {
        this.plannedValue = newPlannedValue;
        return this;
    }

    /**
     * Gets the currency.
     *
     * @return the currency
     */
    public final String getCurrency() {
        return currency;
    }

    /**
     * Sets the currency.
     *
     * @param newCurrency
     *            the new currency
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setCurrency(final String newCurrency) {
        this.currency = newCurrency;
        return this;
    }

    /**
     * Gets the source.
     *
     * @return the source
     */
    public final String getSource() {
        return source;
    }

    /**
     * Sets the source.
     *
     * @param newSource
     *            the new source
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setSource(final String newSource) {
        this.source = newSource;
        return this;
    }

    /**
     * Gets the body.
     *
     * @return the body
     */
    public final ParsedBody getBody() {
        return body;
    }

    /**
     * Sets the body.
     *
     * @param newBody
     *            the new body
     * @return the parsed budget item
     */
    public final ParsedBudgetItem setBody(final ParsedBody newBody) {
        this.body = newBody;
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
    public final ParsedBudgetItem setLevel1Name(final String name) {
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
    public final ParsedBudgetItem setLevel1Code(final String code) {
        this.level1Code = code;
        return this;
    }

    /**
     * @return country
     */
    public final String getCountry() {
        return country;
    }

    /**
     * @param country
     *      country to set
     * @return this instance for chaining
     */
    public final ParsedBudgetItem setCountry(final String country) {
        this.country = country;
        return this;
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
    public final ParsedBudgetItem setReportSubType(final String reportSubType) {
        this.reportSubType = reportSubType;
        return this;
    }
}
