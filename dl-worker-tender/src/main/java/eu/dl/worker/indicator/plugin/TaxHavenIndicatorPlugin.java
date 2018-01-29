package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.time.LocalDate;
import java.util.HashMap;

/**
 * This plugin calculates Tax haven indicator.
 */
public class TaxHavenIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

	/**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();
    
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null || tender.getPublications() == null) {
            return insufficient();
        }

        if (!tender.getPublications()
                .stream()
                .anyMatch(p -> p.getFormType() == PublicationFormType.CONTRACT_AWARD)) {
            // there is no CA publication
            return undefined();
        }

        LocalDate publicationDate = null;
        for (Publication publication : tender.getPublications()) {
	        	if (publication.getFormType() == PublicationFormType.CONTRACT_AWARD) {
	        		if (publication.getPublicationDate() != null) {
		        		if (publicationDate == null || publication.getPublicationDate().isBefore(publicationDate)) {
		        			publicationDate = publication.getPublicationDate();
		        		}
	        		}
	        	}
        }
        if (publicationDate == null) {
            // publication date is not filled
            return insufficient();
        }

        MasterBody winningBidder = null;
        for (MasterTenderLot lot : tender.getLots()) {
            if (lot.getBids() != null) {
                for (MasterBid bid : lot.getBids()) {
                    if (bid.getIsWinning() != null && bid.getIsWinning() && bid.getBidders() != null) {
                        for (MasterBody bidder : bid.getBidders()) {
                            if (bidder.getAddress() != null && bidder.getAddress().getCountry() != null) {
                                winningBidder = bidder;
                                break;
                            }
                        }
                        if (winningBidder != null) {
                            break;
                        }
                    }
                }
                if (winningBidder != null) {
                    break;
                }
            }
        }
        if (winningBidder == null) {
            // country is not filled
            return insufficient();
        }

        HashMap<String, Object> metaData = new HashMap<String, Object>();
        metaData.put("bidderGroupId", winningBidder.getGroupId());
        String configKey = "indicator." + winningBidder.getAddress().getCountry() + ".taxHaven."
                + publicationDate.getYear();
        if (config.getParam(configKey) != null && config.getParam(configKey).equals("YES")) {
            return calculated(0d, metaData);
        } else {
            return calculated(100d, metaData);
        }
    }


    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_TAX_HAVEN.name();
    }

}