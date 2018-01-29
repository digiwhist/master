package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
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

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.INTEGRITY_NEW_COMPANY;

/**
 * This plugin calculates new company indicator.
 */
public class NewCompanyIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null) {
            return insufficient();
        }

        // if the master tender does not have any included publication of type CONTRACT_AWARD, return undefined
        if (tender.getPublications() == null || tender.getPublications()
                .stream()
                .filter(p -> p.getIsIncluded() != null && p.getIsIncluded())
                .noneMatch(p -> p.getFormType() == PublicationFormType.CONTRACT_AWARD)) {
            return undefined();
        }

        // if the tender has a publication of type CONTRACT_AWARD and does not have any winning bid, return insufficient
        if (tender.getPublications()
                .stream()
                .anyMatch(p -> p.getFormType() == PublicationFormType.CONTRACT_AWARD)
                && tender.getLots()
                .stream()
                .noneMatch(l -> l.getBids() != null && l.getBids()
                    .stream()
                    .anyMatch(b -> b.getIsWinning() != null && b.getIsWinning()))) {
            return insufficient();
        }

        // search for the date of the first CONTRACT_AWARD publication
        LocalDate contractAwardPublicationDate = null;
        for (Publication publication: tender.getPublications()) {
            if (publication.getFormType() != null
                    && publication.getFormType() == PublicationFormType.CONTRACT_AWARD
                    && publication.getPublicationDate() != null
                    && (contractAwardPublicationDate == null
                        || contractAwardPublicationDate.isAfter(publication.getPublicationDate()))) {
                contractAwardPublicationDate = publication.getPublicationDate();
            }
        }

        if (contractAwardPublicationDate == null) {
            return insufficient();
        }

        List<String> bidderGroupIds = new ArrayList<>();

        // iterate over winning bids and try to identify those, where is winning bidder younger than one year
        boolean someBidderIsMissingFoundationDate = false;
        for (MasterTenderLot lot : tender.getLots()) {
            if (lot.getBids() != null) {
                for (MasterBid bid : lot.getBids()) {
                    if (bid.getIsWinning() && bid.getBidders() != null) {
                        for (MasterBody bidder : bid.getBidders()) {
                            LocalDate foundationDate = getFoundationDate(bidder);
                            if (foundationDate != null) {
                                if (ChronoUnit.DAYS.between(foundationDate, contractAwardPublicationDate) < 365) {
                                    bidderGroupIds.add(bidder.getGroupId());
                                }
                            } else {
                                someBidderIsMissingFoundationDate = true;
                            }
                        }
                    }
                }
            }
        }

        if (!bidderGroupIds.isEmpty()) {
            // date of contract award - company foundation date < 365 days for at least one bidder, return CALCULATED
            // indicator with value 1
            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("bidderGroupIds", bidderGroupIds);
            return calculated(0d, metaData);
        }

        if (someBidderIsMissingFoundationDate) {
            // at least one bidder is missing company foundation date and date of contract award - company foundation
            // date >= 365 days for all remaining bidders, return insufficient
            return insufficient();
        } else {
            // date of contract award - company foundation date >= 365 days for all bidders, return CALCULATED
            // indicator with value 0
            return calculated(100d);
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
        return INTEGRITY_NEW_COMPANY.name();
    }

}