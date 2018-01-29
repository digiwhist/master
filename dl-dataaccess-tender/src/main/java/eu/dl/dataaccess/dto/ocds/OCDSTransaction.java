package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import java.time.LocalDateTime;

/**
 * OCDS transaction. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSTransaction {
    private String id;

    private LocalDateTime date;

    private OCDSValue value;

    private OCDSOrganizationReference payer;
    
    private OCDSOrganizationReference payee;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSTransaction setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return date
     */
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getDate() {
        return date;
    }

    /**
     * @param date
     *      date to be set
     * @return this instance for chaining
     */
    public final OCDSTransaction setDate(final LocalDateTime date) {
        this.date = date;
        return this;
    }

    /**
     * @return value
     */
    public final OCDSValue getValue() {
        return value;
    }

    /**
     * @param value
     *      value to be set
     * @return this instance for chaining
     */
    public final OCDSTransaction setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }

    /**
     * @return payer
     */
    public final OCDSOrganizationReference getPayer() {
        return payer;
    }

    /**
     * @param payer
     *      payer to be set
     * @return this instance for chaining
     */
    public final OCDSTransaction setPayer(final OCDSOrganizationReference payer) {
        this.payer = payer;
        return this;
    }

    /**
     * @return payee
     */
    public final OCDSOrganizationReference getPayee() {
        return payee;
    }

    /**
     * @param payee
     *      payee to be set
     * @return this instance for chaining
     */
    public final OCDSTransaction setPayee(final OCDSOrganizationReference payee) {
        this.payee = payee;
        return this;
    }
}
