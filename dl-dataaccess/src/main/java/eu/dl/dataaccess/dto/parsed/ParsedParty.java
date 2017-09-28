package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * Party.
 */
public class ParsedParty {

    /**
     * Parsed party name.
     */
    private String party;

    /**
     * Parsed party start date.
     */
    private String startDate;

    /**
     * Parsed party end date.
     */
    private String endDate;


    /**
     * Gets the party.
     *
     * @return the party
     */
    public final String getParty() {
        return party;
    }


    /**
     * Sets the party.
     *
     * @param party
     *            the party
     * @return the parsed party
     */
    public final ParsedParty setParty(final String party) {
        this.party = party;
        return this;
    }


    /**
     * Gets the start date.
     *
     * @return the start date
     */
    public final String getStartDate() {
        return startDate;
    }


    /**
     * Sets the start date.
     *
     * @param startDate
     *            the start date
     * @return the parsed party
     */
    public final ParsedParty setStartDate(final String startDate) {
        this.startDate = startDate;
        return this;
    }


    /**
     * Gets the end date.
     *
     * @return the end date
     */
    public final String getEndDate() {
        return endDate;
    }


    /**
     * Sets the end date.
     *
     * @param endDate
     *            the end date
     * @return the parsed party
     */
    public final ParsedParty setEndDate(final String endDate) {
        this.endDate = endDate;
        return this;
    }
}
