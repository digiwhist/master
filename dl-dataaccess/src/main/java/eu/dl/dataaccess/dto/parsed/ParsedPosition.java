package eu.dl.dataaccess.dto.parsed;

/**
 * Position.
 */
public class ParsedPosition {

    /**
     * Parsed position.
     */
    private String position;

    /**
     * Parsed start date of position.
     */
    private String startDate;

    /**
     * Parsed end date of position.
     */
    private String endDate;


    /**
     * Gets the position.
     *
     * @return the position
     */
    public final String getPosition() {
        return position;
    }


    /**
     * Sets the position.
     *
     * @param position
     *            the position
     * @return the parsed position
     */
    public final ParsedPosition setPosition(final String position) {
        this.position = position;
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
     * @return the parsed position
     */
    public final ParsedPosition setStartDate(final String startDate) {
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
     * @return the parsed position
     */
    public final ParsedPosition setEndDate(final String endDate) {
        this.endDate = endDate;
        return this;
    }
}
