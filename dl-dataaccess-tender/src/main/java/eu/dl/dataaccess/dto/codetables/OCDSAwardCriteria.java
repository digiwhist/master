package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS award criteria enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSAwardCriteria {
    /**
     * Price.
     */
    PRICE_ONLY,

    /**
     * Meat.
     */
    RATED_CRITERIA;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }

    /**
     * Returns OCDS award criteria for the given tender selection method.
     *
     * @param value
     *      tender selection method
     * @return OCDS award criteria
     */
    public static OCDSAwardCriteria from(final SelectionMethod value) {
        if (value == null) {
            return null;
        }

        switch (value) {
            case LOWEST_PRICE:
                return PRICE_ONLY;
            case MEAT:
                return RATED_CRITERIA;
            default:
                return null;
        }
    }
}
