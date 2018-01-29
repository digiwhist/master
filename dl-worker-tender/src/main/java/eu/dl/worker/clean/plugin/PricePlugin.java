package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.PriceUtils;


/**
 * Plugin used to clean prices.
 *
 * @author Tomas Mrazek
 */
public class PricePlugin extends BaseNumberPlugin<ParsedTender, CleanTender> {
    /**
     * Constructor with number format.
     * 
     * @param format
     *            number format
     */
    public PricePlugin(final NumberFormat format) {
        super(format);
    }

    /**
     * Constructor with list of number formats.
     * 
     * @param formats
     *            number formats
     */
    public PricePlugin(final List<NumberFormat> formats) {
        super(formats);
    }

    /**
     * Cleans prices.
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
        String country = parsedTender.getCountry();

        logger.debug("Cleaning document price for parsed tender {} starts", parsedTender.getId());
        cleanTender.setDocumentsPrice(PriceUtils.cleanPrice(parsedTender.getDocumentsPrice(), formats, country));
        logger.debug("Cleaning document price for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning estimated price for parsed tender {} starts", parsedTender.getId());
        cleanTender.setEstimatedPrice(PriceUtils.cleanPrice(parsedTender.getEstimatedPrice(), formats, country));
        logger.debug("Cleaning estimated price for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning final price for parsed tender {} starts", parsedTender.getId());
        cleanTender.setFinalPrice(PriceUtils.cleanPrice(parsedTender.getFinalPrice(), formats, country));
        logger.debug("Cleaning final price for parsed tender {} finished", parsedTender.getId());

        return cleanTender;
    }
}
