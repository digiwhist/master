package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.OCDSBidStatisticMeasure;

/**
 * OCDS bid statistic. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/extensions/bids/">Bid statistics and details Extension</a>
 */
@Transformable
public class OCDSBidStatistic {

    private String id;

    // codelist value according to mapping
    private OCDSBidStatisticMeasure measure;

    private Integer value;

    private String relatedLot;

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
    public final OCDSBidStatistic setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return measure
     */
    public final OCDSBidStatisticMeasure getMeasure() {
        return measure;
    }

    /**
     * @param measure
     *      measure to be set
     * @return this instance for chaining
     */
    public final OCDSBidStatistic setMeasure(final OCDSBidStatisticMeasure measure) {
        this.measure = measure;
        return this;
    }

    /**
     * @return value
     */
    public final Integer getValue() {
        return value;
    }

    /**
     * @param value
     *      value to be set
     * @return this instance for chaining
     */
    public final OCDSBidStatistic setValue(final Integer value) {
        this.value = value;
        return this;
    }

    /**
     * @return related lot
     */
    public final String getRelatedLot() {
        return relatedLot;
    }

    /**
     * @param relatedLot
     *      related lot to be set
     * @return this instance for chaining
     */
    public final OCDSBidStatistic setRelatedLot(final String relatedLot) {
        this.relatedLot = relatedLot;
        return this;
    }
}
