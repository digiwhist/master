package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * Funding. Funding sources used (if none listed, implicitly assuming own budget
 * of the body..).
 */
public class ParsedFunding {
    /**
     * Name of the funding source.
     */
    private String source;

    /**
     * Source is from EU funds.
     */
    private String isEuFund;

    /**
     * Refined classification of sources (such as type of Operational programme
     * etc..).
     */
    private String programme;

    /**
     * Amount of money contributed from the source.
     */
    private ParsedPrice amount;

    /**
     * Percentage of money contributed from the source.
     */
    private String proportion;

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
     * @param source
     *            the source
     * @return the parsed funding
     */
    public final ParsedFunding setSource(final String source) {
        this.source = source;
        return this;
    }

    /**
     * Gets the checks if is eu fund.
     *
     * @return the checks if is eu fund
     */
    public final String getIsEuFund() {
        return isEuFund;
    }

    /**
     * Sets the is eu fund.
     *
     * @param isEuFund
     *            the is eu fund
     * @return the parsed funding
     */
    public final ParsedFunding setIsEuFund(final String isEuFund) {
        this.isEuFund = isEuFund;
        return this;
    }

    /**
     * Gets the programme.
     *
     * @return the programme
     */
    public final String getProgramme() {
        return programme;
    }

    /**
     * Sets the programme.
     *
     * @param programme
     *            the programme
     * @return the parsed funding
     */
    public final ParsedFunding setProgramme(final String programme) {
        this.programme = programme;
        return this;
    }

    /**
     * Gets the amount.
     *
     * @return the amount
     */
    public final ParsedPrice getAmount() {
        return amount;
    }

    /**
     * Sets the amount.
     *
     * @param amount
     *            the amount
     * @return the parsed funding
     */
    public final ParsedFunding setAmount(final ParsedPrice amount) {
        this.amount = amount;
        return this;
    }

    /**
     * Gets the proportion.
     *
     * @return the proportion
     */
    public final String getProportion() {
        return proportion;
    }

    /**
     * Sets the proportion.
     *
     * @param proportion
     *            the proportion
     * @return the parsed funding
     */
    public final ParsedFunding setProportion(final String proportion) {
        this.proportion = proportion;
        return this;
    }
}
