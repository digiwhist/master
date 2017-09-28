package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.CORRUPTION_NEW_COMPANY;

/**
 * This plugin calculates Single bid indicator.
 */
public class NewCompanyIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null || tender.getPublications() == null) {
            return null;
        }

        Indicator indicator = new BasicEntityRelatedIndicator();
        indicator.setType(getType());
        List<String> bidderGroupIds = new ArrayList<String>();

        // search for the date of the first CONTRACT_AWARD publication
        LocalDate contractAwardPublicationDate = null;
        for (Publication publication: tender.getPublications()) {
            if (publication.getFormType() != null
                    && publication.getFormType().equals(PublicationFormType.CONTRACT_AWARD)) {
                if (publication.getPublicationDate() != null
                        && (
                            contractAwardPublicationDate == null
                            || contractAwardPublicationDate.isAfter(publication.getPublicationDate())
                        )
                    ) {
                    contractAwardPublicationDate = publication.getPublicationDate();
                }
            }
        }

        if (contractAwardPublicationDate == null) {
            // no publication of type CONTRACT_AWARD or no date available
            return null;
        }

        // iterate over winning bids and try to identify those,
        // where is winning bidder younger than one year
        for (MasterTenderLot lot : tender.getLots()) {
            if (lot.getBids() != null) {
                for (MasterBid bid : lot.getBids()) {
                    if (bid.getIsWinning() && bid.getBidders() != null) {
                        for (MasterBody bidder : bid.getBidders()) {
                            LocalDate foundationDate = getFoundationDate(bidder);
                            if (foundationDate != null
                                    && ChronoUnit.DAYS.between(foundationDate, contractAwardPublicationDate) < 365
                                    && ChronoUnit.DAYS.between(foundationDate, contractAwardPublicationDate) > 0) {
                                bidderGroupIds.add(bidder.getGroupId());
                            }
                        }
                    }
                }

            }
        }

        if (!bidderGroupIds.isEmpty()) {
            // at least one lot has only one bid

            // store to metadata lotIds with the single bid
            HashMap<String, Object> metaData = new HashMap<String, Object>();
            metaData.put("bidderGroupIds", bidderGroupIds);
            indicator.setMetaData(metaData);

            return indicator;
        } else {
            return null;
        }
    }

    /**
     * Gets foundation date from the bidder.
     * @param bidder bidder
     * @return foundation date or null
     */
    private LocalDate getFoundationDate(final MasterBody bidder) {
        if (bidder == null || bidder.getMetaData() == null) {
            return null;
        }

        if (bidder.getMetaData().containsKey("foundationDate")) {
            String rawDate = bidder.getMetaData().get("foundationDate").toString();
            if (rawDate != null && !rawDate.isEmpty()) {
                if (rawDate.length() == 4) {
                    return LocalDate.parse(rawDate, new DateTimeFormatterBuilder()
                            .appendPattern("yyyy")
                            .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                            .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1)
                            .toFormatter());
                } else if (rawDate.length() == 6) {
                    return LocalDate.parse(rawDate, new DateTimeFormatterBuilder()
                            .appendPattern("yyyyMM")
                            .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                            .toFormatter());
                } else if (rawDate.length() == 8) {
                    return LocalDate.parse(rawDate, DateTimeFormatter.ofPattern("yyyyMMdd"));
                }
            }
        }

        return null;
    }

    @Override
    public final String getType() {
        return CORRUPTION_NEW_COMPANY.name();
    }

}