package eu.dl.worker.indicator.plugin;

import java.time.LocalDate;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * This plugin calculates Single bid indicator.
 */
public class TaxHavenIndicatorPlugin implements IndicatorPlugin<MasterTender> {

	/**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();
    
    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null || tender.getPublications() == null) {
            return null;
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
        
        if (publicationDate != null) {
        	for (MasterTenderLot lot : tender.getLots()) {
        		if (lot.getBids() != null) {
        			for (MasterBid bid : lot.getBids()) {
        				if (bid.getBidders() != null && !bid.getBidders().isEmpty()) {
        					for (MasterBody bidder : bid.getBidders()) {
	        					if (bidder.getAddress() != null && bidder.getAddress().getCountry() != null) {
	        						String configKey = "indicator." 
	        								+ bidder.getAddress().getCountry() 
	        								+ ".taxHaven." + publicationDate.getYear();
	        						if (config.getParam(configKey) != null 
	        								&& config.getParam(configKey).equals("YES")) {
	        							Indicator indicator = new BasicEntityRelatedIndicator();
	        							indicator.setType(getType());
	        					
	        							return indicator;
	        						}
	        					}
        					}
        				}
        			}
        		}
        	}
        }
        
        return null;
    }


    @Override
    public final String getType() {
        return TenderIndicatorType.CORRUPTION_TAX_HAVEN.name();
    }

}