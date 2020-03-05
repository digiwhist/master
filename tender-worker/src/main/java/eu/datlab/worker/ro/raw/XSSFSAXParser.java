package eu.datlab.worker.ro.raw;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.BiConsumer;

import eu.dl.core.UnrecoverableException;
import org.apache.poi.ooxml.util.SAXHelper;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.model.SharedStringsTable;
import org.apache.poi.xssf.usermodel.XSSFRichTextString;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import javax.xml.parsers.ParserConfigurationException;

/**
 * XLSX parser.
 */
public final class XSSFSAXParser {
    private final BiConsumer<Integer, List<String>> rowHandler;

    /**
     * Constructor with row handler initialization.
     *
     * @param handler
     *      row handler to be set, accepts two parameters, row number and row data
     */
    public XSSFSAXParser(final BiConsumer<Integer, List<String>> handler) {
        this.rowHandler = handler;
    }

    /**
     * Default constructor without row handler.
     */
    public XSSFSAXParser() {
        this.rowHandler = null;
    }

    /**
     * Processes the first sheet.
     *
     * @param path
     *      file path
     * @throws Exception
     *       Workbook parsing error.
     */
    public void processFirstSheet(final String path) throws Exception {
        OPCPackage pkg = OPCPackage.open(path);
        XSSFReader r = new XSSFReader(pkg);
        SharedStringsTable sst = r.getSharedStringsTable();
        XMLReader parser = fetchSheetParser(sst);
        // To look up the Sheet Name / Sheet Order / rID,
        //  you need to process the core Workbook stream.
        // Normally it's of the form rId# or rSheet#

        Iterator<InputStream> sheets = r.getSheetsData();
        if (sheets.hasNext()) {
            InputStream sheet = sheets.next();
            InputSource sheetSource = new InputSource(sheet);
            parser.parse(sheetSource);
            sheet.close();
        }
    }

    /**
     * Processes all sheets.
     *
     * @param path
     *      file path
     * @throws Exception
     *      Workbook parsing error
     */
    public void processAllSheets(final String path) throws Exception {
        OPCPackage pkg = OPCPackage.open(path);
        XSSFReader r = new XSSFReader(pkg);
        SharedStringsTable sst = r.getSharedStringsTable();
        XMLReader parser = fetchSheetParser(sst);
        Iterator<InputStream> sheets = r.getSheetsData();
        while (sheets.hasNext()) {
            InputStream sheet = sheets.next();

            InputSource sheetSource = new InputSource(sheet);
            parser.parse(sheetSource);
            sheet.close();
        }
    }

    /**
     * @param sst
     *      shared strings table
     * @return parser
     * @throws SAXException
     *      Any SAX exception, possibly wrapping another exception.
     * @throws ParserConfigurationException
     *      Serious configuration error.
     */
    private XMLReader fetchSheetParser(final SharedStringsTable sst) throws SAXException, ParserConfigurationException {
        XMLReader parser = SAXHelper.newXMLReader();
        ContentHandler handler = new SheetHandler(sst, rowHandler);
        parser.setContentHandler(handler);
        return parser;
    }

    /**
     * See org.xml.sax.helpers.DefaultHandler javadocs.
     */
    private static final class SheetHandler extends DefaultHandler {
        private SharedStringsTable sst;
        private StringBuffer value = new StringBuffer();
        private XSSFDataType nextDataType;
        private List<String> currentRow;
        private Integer currentRowNumber;
        private BiConsumer<Integer, List<String>> rowHandler;
        private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

        /**
         * XLSX cell data types.
         */
        enum XSSFDataType {
            BOOL, ERROR, FORMULA, INLINESTR, SSTINDEX, NUMBER;

            /**
             * @param type
             *      cell data type
             * @return enum for the given cell data type
             */
            public static XSSFDataType getFromCellType(final String type) {
                if (type == null) {
                    return XSSFDataType.NUMBER;
                }

                switch(type) {
                    case "b":
                        return XSSFDataType.BOOL;
                    case "e":
                        return XSSFDataType.ERROR;
                    case "inlineStr":
                        return XSSFDataType.INLINESTR;
                    case "s":
                        return XSSFDataType.SSTINDEX;
                    case "str":
                        return XSSFDataType.FORMULA;
                    default:
                        return XSSFDataType.NUMBER;
                }
            }
        }

        /**
         * Default constructor.
         *
         * @param sst
         *      shared string table
         * @param rowHandler
         *      row handler
         */
        private SheetHandler(final SharedStringsTable sst, final BiConsumer<Integer, List<String>> rowHandler) {
            this.sst = sst;
            this.rowHandler = rowHandler;
        }

        @Override
        public void startElement(final String uri, final String localName, final String name, final Attributes attributes)
                throws SAXException {
            String cellType = attributes.getValue("t");
            value = new StringBuffer();

            if (name.equals("c")) {
                nextDataType = XSSFDataType.getFromCellType(cellType);
            } else if (name.equals("row")) {
                currentRow = new ArrayList<>();
                currentRowNumber = Integer.valueOf(attributes.getValue("r"));
            }
        }

        @Override
        public void endElement(final String uri, final String localName, final String name) throws SAXException {
            // Output after we've seen the string contents
            Object cellValue = "";
            if (name.equals("c")) {
                switch (nextDataType) {
                    case BOOL:
                        currentRow.add(value.charAt(0) != '0' ? String.valueOf(true) :  String.valueOf(false));
                        break;
                    case ERROR:
                        currentRow.add("ERROR: \"" + value.toString() + '"');
                        break;
                    case FORMULA:
                        // A formula could result in a string value,
                        // so always add double-quote characters.
                        currentRow.add('"' + value.toString() + '"');
                        break;
                    case INLINESTR:
                        XSSFRichTextString rtsi = new XSSFRichTextString(value.toString());
                        currentRow.add(rtsi.toString());
                        break;
                    case SSTINDEX:
                        String strIndex = value.toString();
                        try {
                            int idx = Integer.parseInt(strIndex);
                            currentRow.add(sst.getItemAt(idx).getString());
                        } catch (NumberFormatException ex) {
                            logger.error("Unable to parse SST index {} because of", strIndex, ex);
                            throw new UnrecoverableException("Unable to parse SST index");
                        }
                        break;
                    case NUMBER:
//                        String n = value.toString();
//                        if (this.formatString != null)
//                            thisStr = formatter.formatRawCellContents(Double
//                                    .parseDouble(n), this.formatIndex,
//                                this.formatString);
//                        else
//                            thisStr = n;

                        currentRow.add(value.toString());
                        break;
                    default:
                        logger.warn("Unexpected data type {}", nextDataType);
                        currentRow.add(value.toString());
                        break;
                }
            } else if (name.equals("row")) {
                if (rowHandler != null) {
                    rowHandler.accept(currentRowNumber, currentRow);
                }
            }
        }

        @Override
        public void characters(final char[] ch, final int start, final int length) {
            value.append(ch, start, length);
        }
    }
}
