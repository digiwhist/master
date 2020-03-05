package eu.datlab.worker.ug.raw;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;

import java.io.File;
import java.io.IOException;

/**
 * Crawler for Uganda - transparency international data.
 */
public final class GPPTITenderCrawler extends BaseCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_URL = PublicationSources.UG_GPP_TI;

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void doWork(final Message message) {
        String path = message.getValue("file");

        int row = 0;

        try {
            final JsonFactory factory = new JsonFactory();
            final JsonParser parser = factory.createParser(new File(path));

            ObjectMapper objMap = new ObjectMapper(factory);
            parser.setCodec(objMap);

            while (!parser.isClosed()) {
                final JsonToken jsonToken = parser.nextToken();

                if (JsonToken.START_ARRAY.equals(jsonToken)) {
                    continue;
                }

                if (JsonToken.START_OBJECT.equals(jsonToken)) {
                    row++;
                    JsonNode node = parser.readValueAsTree();
                    String country = node.get("country").asText(null);
                    if ("UG".equals(country)) {
                        createAndPublishMessage(SOURCE_URL + "?row=" + row, node.toString());
                    }
                }
            }
        } catch (IOException ex) {
            logger.error("Unable to parse json {}.", path, ex);
            throw new UnrecoverableException("Unable to parse json");
        }
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
