package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS lot bid extension. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/extensions/bids/">Bid statistics and details Extension</a>
 */
@Transformable
public class OCDSBid extends BaseOCDSExtension {
    /**
     * Bid Extension URL.
     */
    public static final String EXTENSION_URL =
        "https://raw.githubusercontent.com/open-contracting/ocds_bid_extension/v1.1.1/extension.json";

    private List<OCDSBidStatistic> statistics;
    
    private List<OCDSBidDetail> details;

    @Override
    public final String getUrl() {
        return EXTENSION_URL;
    }

    /**
     * @return list of statistics
     */
    public final List<OCDSBidStatistic> getStatistics() {
        return statistics;
    }

    /**
     *
     * @param statistics
     *      list of statistics to be set
     * @return this instance for chaining
     */
    public final OCDSBid setStatistics(final List<OCDSBidStatistic> statistics) {
        this.statistics = statistics;
        return this;
    }

    /**
     * Adds statistic. List is created if needed.
     *
     * @param statistic
     *      statistic to be added
     * @return this instance for chaining
     */
    public final OCDSBid addStatistic(final OCDSBidStatistic statistic) {
        if (statistic != null) {
            if (this.statistics == null) {
                this.statistics = new ArrayList<>();
            }

            this.statistics.add(statistic);
        }

        return this;
    }

    /**
     * Adds list of statistics. List is created if needed.
     *
     * @param newStatistics
     *      list of the statistics to be added
     * @return this instance for chaining
     */
    public final OCDSBid addStatistics(final List<OCDSBidStatistic> newStatistics) {
        if (newStatistics != null) {
            if (this.statistics == null) {
                this.statistics = new ArrayList<>();
            }

            this.statistics.addAll(newStatistics);
        }

        return this;
    }

    /**
     * @return list of details
     */
    public final List<OCDSBidDetail> getDetails() {
        return details;
    }

    /**
     * @param details
     *      list fo details to be set
     * @return this instance for chaining
     */
    public final OCDSBid setDetails(final List<OCDSBidDetail> details) {
        this.details = details;
        return this;
    }

    /**
     * Adds detail. List is created if needed.
     *
     * @param detail
     *      detail to be added
     * @return this instance for chaining
     */
    public final OCDSBid addDetail(final OCDSBidDetail detail) {
        if (detail != null) {
            if (this.details == null) {
                this.details = new ArrayList<>();
            }

            this.details.add(detail);
        }

        return this;
    }
}
