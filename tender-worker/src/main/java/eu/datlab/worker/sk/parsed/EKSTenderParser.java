package eu.datlab.worker.sk.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type.ORGANIZATION_ID;
import static eu.dl.dataaccess.dto.codetables.BodyIdentifier.Scope.SK;


/**
 * Parses EKS csv.
 *
 * @author Tomas Mrazek
 */
public final class EKSTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    private static final Logger logger = LoggerFactory.getLogger(EKSTenderParser.class);

    private static final CSVFormat CSV_FORMAT = CSVFormat.RFC4180.withDelimiter('|').withQuote('"');

    private static final String METADATA_HEADER_KEY = "header";

    /**
     * Parses the given raw tender object.
     *
     * @param raw
     *         raw tender to be parsed
     *
     * @return list of parsed tender items
     */
    @Override
    public List<ParsedTender> parse(final RawData raw) {
        logger.info("Trying to parse csv {} from {}.", raw.getSourceFileName(), raw.getSourceUrl());

        if (raw.getMetaData() == null || !raw.getMetaData().containsKey(METADATA_HEADER_KEY)) {
            logger.error("Insufficient data, a CSV header is missing.");
            throw new UnrecoverableException("Insufficient data, a CSV header is missing.");
        }
        List<String> header = (List<String>) raw.getMetaData().get(METADATA_HEADER_KEY);
        try {
            final CSVParser parser = CSVParser.parse(raw.getSourceData(), CSV_FORMAT.withHeader(header.toArray(new String[header.size()])));

            final List<CSVRecord> records = parser.getRecords();

            List<ParsedTender> parsed = new ArrayList<>();
            for (CSVRecord r : records) {
                parsed.add(new ParsedTender()
                    .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(raw.getSourceUrl().toString())
                        .setSourceTenderId(r.get("eks_cislozakazky"))
                        .setSourceFormType(PublicationFormType.CONTRACT_AWARD.name())
                        .setHumanReadableUrl(r.get("order_url")))
                    .setTitle(r.get("order_name"))
                    .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(r.get("hlavnyCPV")))
                    .setAwardDecisionDate(r.get("contractdate"))
                    .addBuyer(new ParsedBody()
                        .setName(r.get("obstaravatel_meno"))
                        .addBodyId(getBodyId(r.get("obstaravatel_ico"))))
                    .addLot(new ParsedTenderLot()
                        .setBidsCount(r.get("applicantcount"))
                        .setElectronicBidsCount(r.get("offercountelectronic"))
                        .setOtherEuMemberStatesCompaniesBidsCount(r.get("offereu"))
                        .setNonEuMemberStatesCompaniesBidsCount(r.get("offernoneu"))
                        .addBid(new ParsedBid()
                            .setIsWinning(String.valueOf(true))
                            .setPrice(new ParsedPrice()
                                .setNetAmount(r.get("suma"))
                                .setCurrency("EUR")
                                .setVat(r.get("dph")))
                            .addBidder(new ParsedBody()
                                .setName(r.get("dodavatel_meno"))
                                .addBodyId(getBodyId(r.get("dodavatel_ico"))))))
                );
            }

            return parsed;
        } catch (IOException e) {
            logger.error("Unable to parse CSV {} because of exception", raw.getSourceFileName(), e);
            throw new UnrecoverableException("Unable to parse CSV.", e);
        }
    }

    /**
     * @param id
     *      body id
     * @return BodyIdentifier instance
     */
    private static BodyIdentifier getBodyId(final String id) {
        return new BodyIdentifier().setType(ORGANIZATION_ID).setScope(SK).setId(id);
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "SK";
    }
}
