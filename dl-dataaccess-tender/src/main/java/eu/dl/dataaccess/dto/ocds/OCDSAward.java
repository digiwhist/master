package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS award. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSAward extends BaseOCDSLotsAndDocumentsReferrer<OCDSAward> {
    private String id;

    private LocalDateTime date;

    private String relatedBid;

    private OCDSValue value;

    private List<OCDSOrganizationReference> suppliers;

    private List<OCDSItem> items;

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
    public final OCDSAward setId(final String id) {
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
    public final OCDSAward setDate(final LocalDateTime date) {
        this.date = date;
        return this;
    }

    /**
     * @return related bid
     */
    public final String getRelatedBid() {
        return relatedBid;
    }

    /**
     * @param relatedBid
     *      related bid to be set
     * @return this instance for chaining
     */
    public final OCDSAward setRelatedBid(final String relatedBid) {
        this.relatedBid = relatedBid;
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
    public final OCDSAward setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }

    /**
     * @return list of suppliers
     */
    public final List<OCDSOrganizationReference> getSuppliers() {
        return suppliers;
    }

    /**
     * @param suppliers
     *      list of suppliers to be set
     * @return this instance for chaining
     */
    public final OCDSAward setSuppliers(final List<OCDSOrganizationReference> suppliers) {
        this.suppliers = suppliers;
        return this;
    }

    /**
     * Adds supplier. List is created if needed.
     *
     * @param supplier
     *      supplier to be added
     * @return this instance for chaining
     */
    public final OCDSAward addSupplier(final OCDSOrganizationReference supplier) {
        if (supplier != null) {
            if (this.suppliers == null) {
                this.suppliers = new ArrayList<>();
            }

            this.suppliers.add(supplier);
        }

        return this;
    }

    /**
     * @return items
     */
    public final List<OCDSItem> getItems() {
        return items;
    }

    /**
     * @param items
     *      list of items to be set
     * @return this instance for chaining
     */
    public final OCDSAward setItems(final List<OCDSItem> items) {
        this.items = items;
        return this;
    }

    /**
     * Adds item. List is created if needed.
     *
     * @param item
     *      item to be added
     * @return this instance for chaining
     */
    public final OCDSAward addItem(final OCDSItem item) {
        if (item != null) {
            if (this.items == null) {
                this.items = new ArrayList<>();
            }

            this.items.add(item);
        }

        return this;
    }

    /**
     * Adds items. List is created if needed.
     *
     * @param newItems
     *      list of items to be added
     * @return this instance for chaining
     */
    public final OCDSAward addItems(final List<OCDSItem> newItems) {
        if (newItems != null && !newItems.isEmpty()) {
            if (this.items == null) {
                this.items = new ArrayList<>();
            }

            this.items.addAll(newItems);
        }

        return this;
    }

    /**
     * Adds suppliers. List is created if needed.
     *
     * @param newSuppliers
     *      list of suppliers to be added
     * @return this instance for chaining
     */
    public final OCDSAward addSuppliers(final List<OCDSOrganizationReference> newSuppliers) {
        if (newSuppliers != null && !newSuppliers.isEmpty()) {
            if (this.suppliers == null) {
                this.suppliers = new ArrayList<>();
            }

            this.suppliers.addAll(newSuppliers);
        }

        return this;
    }
}
