package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import eu.dl.dataaccess.dto.clean.CleanBid;
import eu.dl.dataaccess.dto.parsed.ParsedBid;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;


/**
 * This class provides method for Bid cleaning.
 *
 * @author Tomas Mrazek
 */
public final class BidUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private BidUtils() {

    }

    /**
     * Cleans the given bid.
     *
     * @param parsedBid
     *          parsed bid
     * @param numberFormats
     *          list of number formats
     * @param formatters
     *          date and datetime formatters
     * @param documentTypeMapping
     *          document type mapping
     * @param unitPriceMapping
     *          unit price mapping
     * @param countryMapping
     *          country mapping
     * @param country
     *          country
     * @return cleaned bid
     */
    public static CleanBid cleanBid(final ParsedBid parsedBid, final List<NumberFormat> numberFormats,
            final List<DateTimeFormatter> formatters, final Map<Enum, List<String>> documentTypeMapping,
            final Map<Enum, List<String>> unitPriceMapping, final Map<Enum, List<String>> countryMapping,
            final String country) {
        if (parsedBid == null) {
            return null;
        }

        return new CleanBid()
            //cleaning of the bidId isn't needed but is necessary to set it
            .setBidId(parsedBid.getBidId())
            .setAnnualPriceYearsCount(NumberUtils.cleanInteger(parsedBid.getAnnualPriceYearsCount(), numberFormats))
            //bidder doesn't specify the main activity type and the buyer type. Mappings should be null.
            .setBidders(ArrayUtils.walk(parsedBid.getBidders(),
                (parsedBidder) -> BodyUtils.cleanBody(parsedBidder, null, null,
                        countryMapping)))
            .setDisqualificationReason(StringUtils.cleanLongString(parsedBid.getDisqualificationReason()))
            .setDocuments(ArrayUtils.walk(parsedBid.getDocuments(),
                (parsedDocument) -> DocumentUtils.cleanDocument(parsedDocument, numberFormats, formatters,
                    documentTypeMapping)))
            .setIsConsortium(StringUtils.cleanBoolean(parsedBid.getIsConsortium()))
            .setIsDisqualified(StringUtils.cleanBoolean(parsedBid.getIsDisqualified()))
            .setIsSubcontracted(StringUtils.cleanBoolean(parsedBid.getIsSubcontracted()))
            .setIsWinning(StringUtils.cleanBoolean(parsedBid.getIsWinning()))
            .setMonthlyPriceMonthsCount(NumberUtils.cleanInteger(parsedBid.getMonthlyPriceMonthsCount(), numberFormats))
            .setPayments(ArrayUtils.walk(parsedBid.getPayments(),
                (parsedPayment) -> PaymentUtils.cleanPayment(parsedPayment, numberFormats, formatters, country)))
            .setPrice(PriceUtils.cleanPrice(parsedBid.getPrice(), numberFormats, country))
            .setSubcontractedValue(PriceUtils.cleanPrice(parsedBid.getSubcontractedValue(), numberFormats, country))
            .setSubcontractedProportion(NumberUtils.cleanInteger(parsedBid.getSubcontractedProportion(), numberFormats))
            //subcontractor doesn't specify the main activity type and the buyer type. Mappings should be null.
            .setSubcontractors(ArrayUtils.walk(parsedBid.getSubcontractors(),
                (parsedSubcontractor) -> BodyUtils.cleanBody(parsedSubcontractor, null, null)))
            .setUnitPrices(ArrayUtils.walk(parsedBid.getUnitPrices(),
                (parsedUnitPrice) -> PriceUtils.cleanUnitPrice(parsedUnitPrice, numberFormats, unitPriceMapping)))
            .setWasFinishedOnTime(StringUtils.cleanBoolean(parsedBid.getWasFinishedOnTime()))
            .setWasForEstimatedValue(StringUtils.cleanBoolean(parsedBid.getWasForEstimatedValue()))
            .setWasInRequestedQuality(StringUtils.cleanBoolean(parsedBid.getWasInRequestedQuality()));
    }

    /**
     * Cleans the given bid.
     *
     * @param parsedBid
     *          parsed bid
     * @param numberFormat
     *          number format
     * @param formatter
     *          datetime formatter
     * @param documentTypeMapping
     *          document type mapping
     * @param unitPriceMapping
     *          unit price mapping
     * @param countryMapping
     *          country mapping
     * @param country
     *          country
     * @return cleaned bid
     */
    public static CleanBid cleanBid(final ParsedBid parsedBid, final NumberFormat numberFormat,
            final List<DateTimeFormatter> formatter, final Map<Enum, List<String>> documentTypeMapping,
            final Map<Enum, List<String>> unitPriceMapping, final Map<Enum, List<String>> countryMapping,
            final String country) {
        return cleanBid(parsedBid, Arrays.asList(numberFormat), formatter, documentTypeMapping, unitPriceMapping,
                countryMapping, country);
    }
}
