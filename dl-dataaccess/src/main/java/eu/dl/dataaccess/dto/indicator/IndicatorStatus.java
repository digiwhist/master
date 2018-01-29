package eu.dl.dataaccess.dto.indicator;

/**
 * Enum with possible indicator states.
 */
public enum IndicatorStatus {
    /**
     * Calculated, value available.
     */
    CALCULATED,

    /**
     * Not defined/makes no sense for this entity.
     */
    UNDEFINED,

    /**
     * Cannot be calculated, there is not enough data available.
     */
    INSUFFICIENT_DATA,
}

