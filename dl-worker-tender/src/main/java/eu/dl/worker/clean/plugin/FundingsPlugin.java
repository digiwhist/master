package eu.dl.worker.clean.plugin;


import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CleanUtils;
import eu.dl.worker.clean.utils.FundingUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean fundings codes.
 *
 * @author Tomas Mrazek
 */
public class FundingsPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    /**
     * Number formats.
     */
    private List<NumberFormat> numberFormat;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
     */
    public FundingsPlugin(final NumberFormat numberFormat) {
        this.numberFormat = Arrays.asList(numberFormat);
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      list of number formats
     */
    public FundingsPlugin(final List<NumberFormat> numberFormat) {
        this.numberFormat = numberFormat;
    }

    /**
     * Cleans fundings.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getFundings() != null) {
            logger.debug("Cleaning fundings in parsed tender {} starts", parsedTender.getId());
            cleanTender.setFundings(ArrayUtils.walk(parsedTender.getFundings(),
                (parsedFunding) -> FundingUtils.cleanFunding(parsedFunding, numberFormat,
                    CleanUtils.getParsedItemCountry(parsedTender))));
            logger.debug("Cleaning fundings in parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
