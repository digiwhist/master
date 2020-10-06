package eu.datlab.worker.global.parsed;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import eu.datlab.worker.parser.BasePublicOfficialParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedParty;
import eu.dl.dataaccess.dto.parsed.ParsedPosition;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Public officials parsed for Political Data Yearbook.
 *
 * @author Michal Riha
 */
public class PdyPublicOfficialParser extends BasePublicOfficialParser {

    private static final String PUBLIC_OFFICIAL_NAME = "Name";
    private static final String PUBLIC_OFFICIAL_GENDER = "Gender";
    private static final String PUBLIC_OFFICIAL_BIRTH = "Year of Birth";
    private static final String PUBLIC_OFFICIAL_POSITION = "Position";
    private static final String PUBLIC_OFFICIAL_PARTY = "Party";
    private static final String PUBLIC_OFFICIAL_START_DATE = "Start";
    private static final String PUBLIC_OFFICIAL_END_DATE = "End";

    private static final String[] CSV_HEADER = {PUBLIC_OFFICIAL_POSITION, PUBLIC_OFFICIAL_START_DATE,
            PUBLIC_OFFICIAL_END_DATE, PUBLIC_OFFICIAL_NAME, PUBLIC_OFFICIAL_BIRTH, PUBLIC_OFFICIAL_GENDER,
            PUBLIC_OFFICIAL_PARTY};

    @Override
    public final List<ParsedPublicOfficial> parse(final RawData rawPublicOfficial) {

        final List<ParsedPublicOfficial> parsedPublicOfficials = new ArrayList<>();

        try {
            final CSVParser csvParser = CSVParser.parse(rawPublicOfficial.getSourceData(),
                    CSVFormat.TDF.withHeader(CSV_HEADER).withSkipHeaderRecord(true));
            final List<CSVRecord> listOfPublicOfficials = csvParser.getRecords();

            for (CSVRecord publicOfficialRecord : listOfPublicOfficials) {
                parsedPublicOfficials.add(
                        new ParsedPublicOfficial().setFullName(publicOfficialRecord.get(PUBLIC_OFFICIAL_NAME))
                                .setGender(publicOfficialRecord.get(PUBLIC_OFFICIAL_GENDER))
                                .setYearOfBirth(publicOfficialRecord.get(PUBLIC_OFFICIAL_BIRTH))
                                .addPosition(new ParsedPosition().setPosition(
                                        publicOfficialRecord.get(PUBLIC_OFFICIAL_POSITION))
                                        .setStartDate(publicOfficialRecord.get(PUBLIC_OFFICIAL_START_DATE))
                                        .setEndDate(publicOfficialRecord.get(PUBLIC_OFFICIAL_END_DATE)))
                                .addParty(
                                        new ParsedParty().setParty(publicOfficialRecord.get(PUBLIC_OFFICIAL_PARTY))
                                                .setStartDate(publicOfficialRecord.get(PUBLIC_OFFICIAL_START_DATE))
                                                .setEndDate(publicOfficialRecord.get(PUBLIC_OFFICIAL_END_DATE))));
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }

        return parsedPublicOfficials;
    }

    @Override
    protected final String getVersion() {
        return null;
    }

    @Override
    protected final List<ParsedPublicOfficial> postProcessSourceSpecificRules(final List<ParsedPublicOfficial> parsed, final RawData raw) {
        return parsed;
    }
}
