package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS bid detail. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/extensions/bids/">Bid statistics and details Extension</a>
 */
@Transformable
public class OCDSBidDetail extends BaseOCDSLotsAndDocumentsReferrer<OCDSBidDetail> {

    private String id;

    private List<OCDSOrganizationReference> tenderers;

    private OCDSValue value;

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
    public final OCDSBidDetail setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return list of tenderers
     */
    public final List<OCDSOrganizationReference> getTenderers() {
        return tenderers;
    }

    /**
     * @param tenderers
     *      list of tenderers to be set
     * @return this instance for chaining
     */
    public final OCDSBidDetail setTenderers(final List<OCDSOrganizationReference> tenderers) {
        this.tenderers = tenderers;
        return this;
    }

    /**
     * Adds tenderer. List is created if needed.
     *
     * @param tenderer
     *      tenderer to be added
     * @return this instance for chaining
     */
    public final OCDSBidDetail addTenderer(final OCDSOrganizationReference tenderer) {
        if (tenderer != null) {
            if (this.tenderers == null) {
                this.tenderers = new ArrayList<>();
            }

            this.tenderers.add(tenderer);
        }

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
    public final OCDSBidDetail setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }
}
