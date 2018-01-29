package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS supply type enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSProcurementCategory {
    /**
     * Works.
     */
    WORKS,
    /**
     * Goods.
     */
    GOODS,
    /**
     * Services.
     */
    SERVICES;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }

    /**
     * Returns OCDS procurement category for the given tender supply type.
     *
     * @param value
     *      tender supply type
     * @return OCDS procurement category
     */
    public static OCDSProcurementCategory from(final TenderSupplyType value) {
        if (value == null) {
            return null;
        }
        
        switch (value) {
            case WORKS:
                return WORKS;
            case SUPPLIES:
                return  GOODS;
            case SERVICES:
                return SERVICES;
            default:
                return null;
        }
    }
}
