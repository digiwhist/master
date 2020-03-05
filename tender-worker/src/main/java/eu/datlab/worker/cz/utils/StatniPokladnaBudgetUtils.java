package eu.datlab.worker.cz.utils;

import eu.datlab.worker.cz.parsed.StatniPokladnaFINM201Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINM202Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINM207Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU101Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU102Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU104Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU105Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU106Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU107Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaFINU108Handler;
import eu.datlab.worker.cz.parsed.StatniPokladnaHandler;
import eu.datlab.worker.cz.parsed.StatniPokladnaMISRISHandler;
import eu.datlab.worker.cz.parsed.StatniPokladnaMISRISZUHandler;
import eu.datlab.worker.cz.parsed.StatniPokladnaPPTHandler;
import eu.datlab.worker.cz.parsed.StatniPokladnaROZVHandler;
import eu.datlab.worker.cz.parsed.StatniPokladnaVYKZZHandler;
import eu.dl.core.UnrecoverableException;
import java.lang.reflect.InvocationTargetException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Common and utility functions for StatniPokladnaBudget workers.
 */
public final class StatniPokladnaBudgetUtils {

    private static final Logger logger = LoggerFactory.getLogger(StatniPokladnaBudgetUtils.class.getName());
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private StatniPokladnaBudgetUtils() {
        throw new AssertionError();
    }

    /**
     * @param fileName
     *      dataset file name
     * @return dataset identificator from source file name
     */
    public static String getDatasetIdentificator(final String fileName) {
        Matcher m = Pattern.compile("(FINM20[1-357]|FINU10[124-8]|ROZV|VYKZZ|MIS\\-RIS(\\-ZU)?|PPT).*")
            .matcher(fileName);

        return m.find() ? m.group(1) : null;
    }

    /**
     * @param fileName
     *      dataset fileName
     * @return dataset handler class
     * @throws UnrecoverableException
     *      in case that {@code fileName} is null or the handler is not implemented for the given file type
     */
    public static Class<? extends StatniPokladnaHandler> getHandlerClass(final String fileName) {
        String datasetIdent = StatniPokladnaBudgetUtils.getDatasetIdentificator(fileName);

        if (datasetIdent == null) {
            logger.error("Dataset identificator is empty.", datasetIdent);
            throw new UnrecoverableException("Dataset identificator is empty.");
        }

        switch (datasetIdent) {
            case "FINM201":
                return StatniPokladnaFINM201Handler.class;
            case "FINM202": case "FINM203": case "FINM205":
                return StatniPokladnaFINM202Handler.class;
            case "FINM207":
                return StatniPokladnaFINM207Handler.class;
            case "MIS-RIS":
                return StatniPokladnaMISRISHandler.class;
            case "MIS-RIS-ZU":
                return StatniPokladnaMISRISZUHandler.class;
            case "VYKZZ":
                return StatniPokladnaVYKZZHandler.class;
            case "ROZV":
                return StatniPokladnaROZVHandler.class;
            case "FINU101":
                return StatniPokladnaFINU101Handler.class;
            case "FINU102":
                return StatniPokladnaFINU102Handler.class;
            case "FINU104":
                return StatniPokladnaFINU104Handler.class;
            case "FINU105":
                return StatniPokladnaFINU105Handler.class;
            case "FINU106":
                return StatniPokladnaFINU106Handler.class;
            case "FINU107":
                return StatniPokladnaFINU107Handler.class;
            case "FINU108":
                return StatniPokladnaFINU108Handler.class;
            case "PPT":
                return StatniPokladnaPPTHandler.class;
            default:
                logger.error("No parser implemented for dataset {}.", datasetIdent);
                throw new UnrecoverableException("Could not create appropriate parsed.");
        }
    }

    /**
     * @see StatniPokladnaBudgetUtils#getHandlerClass(java.lang.String)
     * @param fileName
     *      dataset fileName
     * @return new instance of the handler
     * @throws UnrecoverableException
     *      in case it is unable to get new instance of the handler class
     */
    public static StatniPokladnaHandler getHandler(final String fileName) {
        Class<? extends StatniPokladnaHandler> cls = getHandlerClass(fileName);

        try {
            return cls.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            logger.error("Unable to get instance of the handler {} because of exception", cls.getName(), e);
            throw new UnrecoverableException("Unable to instantiate handler.", e);
        }
    }
}
