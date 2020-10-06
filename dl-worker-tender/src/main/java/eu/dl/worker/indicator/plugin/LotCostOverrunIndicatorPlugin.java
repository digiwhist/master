package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;


/**
     * This plugin calculates corruption single bid indicator.
     */
    public final class LotCostOverrunIndicatorPlugin extends LotIndicatorPlugin {

        @Override
        public Indicator evaluate(final MasterTenderLot lot, final MasterTender tender) {
            if (tender == null  || tender.getCountry() == null
                    || lot == null || lot.getBids() == null || lot.getBids().isEmpty()) {
                return insufficient();
            }

            MasterBid winningBid =
                    lot.getBids().stream().filter(Objects::nonNull)
                            .filter(a -> a.getIsWinning() != null && a.getIsWinning()).findFirst().orElse(null);
            if(winningBid == null || winningBid.getPrice() == null
                    || (lot.getEstimatedPrice() == null && tender.getEstimatedPrice() == null)) {
                return insufficient();
            }

            BigDecimal bidPrice = winningBid.getPrice().getNetAmount();
            if(bidPrice == null) {
                bidPrice = winningBid.getPrice().getNetAmountNational();
            }

            BigDecimal estimatedPrice;
            if(lot.getEstimatedPrice() != null) {
                estimatedPrice = lot.getEstimatedPrice().getNetAmount();
                if(estimatedPrice == null) {
                    estimatedPrice = lot.getEstimatedPrice().getNetAmountNational();
                }
            } else {
                if(tender.getLots() != null && tender.getLots().size() > 1) {
                    return insufficient();
                }
                estimatedPrice = tender.getEstimatedPrice().getNetAmount();
                if(estimatedPrice == null) {
                    estimatedPrice = tender.getEstimatedPrice().getNetAmountNational();
                }
            }


            if(bidPrice == null || estimatedPrice == null) {
                return insufficient();
            }

            if(estimatedPrice.equals(BigDecimal.ZERO)) {
                return undefined();
            }

            BigDecimal overrun = ((bidPrice.subtract(estimatedPrice)).divide(estimatedPrice, 3, RoundingMode.HALF_UP));

            String value = Config.getInstance().getParam(
                    "indicator." + tender.getCountry() + ".costOverrun.length");

            return overrun.compareTo(BigDecimal.ZERO) <= 0 || IndicatorUtils.isValueInPeriod(value, overrun)
                    ? calculated(100d)
                    : calculated(0d);
        }

        @Override
        public String getType() {
            return TenderIndicatorType.INTEGRITY_COST_OVERRUN.name();
        }
    }
