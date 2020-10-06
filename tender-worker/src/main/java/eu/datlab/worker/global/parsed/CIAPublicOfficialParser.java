package eu.datlab.worker.global.parsed;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.itextpdf.awt.geom.Rectangle;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.parser.FilteredTextRenderListener;
import com.itextpdf.text.pdf.parser.LocationTextExtractionStrategy;
import com.itextpdf.text.pdf.parser.PdfTextExtractor;
import com.itextpdf.text.pdf.parser.RegionTextRenderFilter;
import com.itextpdf.text.pdf.parser.RenderFilter;

import eu.datlab.worker.global.PublicOfficialUtils.EuCountry;
import eu.datlab.worker.parser.BasePublicOfficialParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedPosition;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Public officials crawler for CIA.
 *
 * @author Michal Riha
 */
public class CIAPublicOfficialParser extends BasePublicOfficialParser {

    private static final String PARSER_VERSION = "1";

    @Override
    public final List<ParsedPublicOfficial> parse(final RawData rawPublicOfficial) {

        final List<ParsedPublicOfficial> parsedPublicOfficials = new ArrayList<>();

        try {
            final PdfReader pdfDocument = new PdfReader(rawPublicOfficial.getSourceBinaryData());

            for (int pageNumber = 4; pageNumber <= pdfDocument.getNumberOfPages(); pageNumber++) {

                // Get text from PDF
                final String countryText = parseTextFromPageInPdf(57, 700, 2000, 20, pageNumber, pdfDocument).trim();
                final String[] bodyAll = parseTextFromPageInPdf(57, 20, 450, 650, pageNumber, pdfDocument).split("\n");
                final String[] bodyPublicOfficials = parseTextFromPageInPdf(340, 20, 110, 650, pageNumber,
                        pdfDocument).split("\n");

                // Parse only EU countries with data
                if (!EuCountry.contains(countryText) || bodyAll.length == 0 || bodyPublicOfficials.length == 0) {
                    continue;
                }

                // Parse public officials from text
                for (int firstIterator = 0; firstIterator < bodyPublicOfficials.length; firstIterator++) {
                    final ParsedPublicOfficial parsedPublicOfficial = parsePublicOfficial(bodyAll,
                            bodyPublicOfficials, firstIterator);

                    if (parsedPublicOfficial != null) {
                        parsedPublicOfficials.add(parsedPublicOfficial.setCountry(countryText));
                    }
                }
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }

        return parsedPublicOfficials;
    }

    @Override
    protected final String getVersion() {
        return PARSER_VERSION;
    }

    /**
     * Check if public official is in bodyAll array, if so, returns name and position of public official.
     * This method also tries to parse postitions across two lines, this results in some positions be named
     * as two position names conencted, when there is position with no politician assigned.
     *
     * @param bodyAll
     *         Array of strings with all text in body part of PDF
     * @param bodyPublicOfficials
     *         Array of strings with only names of public officials
     * @param firstIterator
     *         index of politician being currently parsed
     *
     * @return ParsedPublicOfficial with name and possition filled, or Null when could not be parsed
     */
    private static ParsedPublicOfficial parsePublicOfficial(final String[] bodyAll,
            final String[] bodyPublicOfficials, final int firstIterator) {

        for (int secondIterator = 0; secondIterator < bodyAll.length; secondIterator++) {

            if (bodyAll[secondIterator].contains(bodyPublicOfficials[firstIterator])) {
                if (firstIterator == 0 || secondIterator == 0 || bodyAll[secondIterator - 1].contains(
                        bodyPublicOfficials[firstIterator - 1])) {
                    return new ParsedPublicOfficial().setFullName(bodyPublicOfficials[firstIterator])
                            .addPosition(new ParsedPosition().setPosition(
                                    bodyAll[secondIterator].replace(bodyPublicOfficials[firstIterator], "")));
                } else {
                    return new ParsedPublicOfficial().setFullName(bodyPublicOfficials[firstIterator])
                            .addPosition(new ParsedPosition().setPosition(
                                    bodyAll[secondIterator - 1] + "" + bodyAll[secondIterator].replace(
                                            bodyPublicOfficials[firstIterator], "")));
                }
            }
        }

        return null;
    }

    /**
     * Get text from PDF document in selected rectangle.
     * If only part of text is in the rectangle, it is parsed whole.
     *
     * @param x
     *         X coordinate of bottom left corner of rectangle
     * @param y
     *         Y coordinate of bottom left corner of rectangle
     * @param width
     *         width of rectangle
     * @param height
     *         height of rectangle
     * @param pageNumber
     *         page to get text from
     * @param pdfDocument
     *         pdf document to get text from
     *
     * @return String in selected area
     * @throws IOException
     *         when PDF cannot be parsed
     */
    private static String parseTextFromPageInPdf(final double x, final double y, final double width,
            final double height, final int pageNumber, final PdfReader pdfDocument) throws IOException {

        final RenderFilter filter = new RegionTextRenderFilter(new Rectangle(x, y, width, height));
        final FilteredTextRenderListener filteredTextRenderListener = new FilteredTextRenderListener(
                new LocationTextExtractionStrategy(), filter);

        return PdfTextExtractor.getTextFromPage(pdfDocument, pageNumber, filteredTextRenderListener);
    }

    @Override
    protected final List<ParsedPublicOfficial> postProcessSourceSpecificRules(final List<ParsedPublicOfficial> parsed, final RawData raw) {
        return parsed;
    }
}
