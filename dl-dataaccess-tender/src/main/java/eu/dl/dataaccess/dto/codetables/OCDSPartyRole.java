package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS party role enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSPartyRole {
    /**
     * Buyer.
     */
    BUYER,
    /**
     * Procuring entity.
     */
    PROCURING_ENTITY,
    /**
     * Supplier.
     */
    SUPPLIER,
    /**
     * Tenderer.
     */
    TENDERER,
    /**
     * Funder.
     */
    FUNDER,
    /**
     * Enquirer.
     */
    ENQUIRER,
    /**
     * Payer.
     */
    PAYER,
    /**
     * Payee.
     */
    PAYEE;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
