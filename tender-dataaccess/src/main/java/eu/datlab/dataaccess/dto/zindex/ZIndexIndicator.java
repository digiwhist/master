package eu.datlab.dataaccess.dto.zindex;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.indicator.BasicIndicator;
import java.time.LocalDate;

/**
 * This class represents the zIndex indicator.
 *
 * @author Tomas Mrazek
 */
@Transformable
@JsonTypeInfo(use= JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class", defaultImpl=ZIndexIndicator.class)
public class ZIndexIndicator extends BasicIndicator {
    private LocalDate dateFrom;

    private LocalDate dateTo;

    private String bodyId;

    private String name;

    /**
     * @return start date of indicator validity
     */
    public final LocalDate getDateFrom() {
        return dateFrom;
    }

    /**
     * @param from
     *      start date of indicator validity dateTo be set
     * @return this instance for chaining
     */
    public final ZIndexIndicator setDateFrom(final LocalDate from) {
        this.dateFrom = from;
        return this;
    }

    /**
     * @return end date of indicator validity
     */
    public final LocalDate getDateTo() {
        return dateTo;
    }

    /**
     * @param to
     *      end date of indicator validity dateTo be set
     * @return this instance for chaining
     */
    public final ZIndexIndicator setDateTo(final LocalDate to) {
        this.dateTo = to;
        return this;
    }

    /**
     * @return body id
     */
    public final String getBodyId() {
        return bodyId;
    }

    /**
     * @param bodyId
     *      body id
     * @return this instance for chaining
     */
    public final ZIndexIndicator setBodyId(final String bodyId) {
        this.bodyId = bodyId;
        return this;
    }

    /**
     * @return indicator name
     */
    public final String getName() {
        return name;
    }

    /**
     *
     * @param name
     *      name of the indicator to be set
     * @return this instance for chaining
     */
    public final ZIndexIndicator setName(final String name) {
        this.name = name;
        return this;
    }
}
