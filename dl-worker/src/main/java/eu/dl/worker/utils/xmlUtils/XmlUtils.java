package eu.dl.worker.utils.xmlUtils;

import net.sf.saxon.TransformerFactoryImpl;
import org.slf4j.LoggerFactory;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.ByteArrayOutputStream;
import java.io.StringReader;

/**
 * Utils for XML formatted Strings.
 */
public final class XmlUtils {
    /**
     * Private constructor to make class static.
     */
    private XmlUtils() {
    }

    /**
     * Format XML with XSL.
     *
     * @param xslString XSL to format with
     * @param xmlString XML to format
     *
     * @return formated XML String
     */
    public static String formatXmlWithXsl(final String xslString, final String xmlString) {
        try {
            // Load xsl file to transform xml file with
            final Transformer xslTransformer = TransformerFactoryImpl.newInstance()
                    .newTransformer(new StreamSource(new StringReader(xslString)));

            // Load xml file to be transformed
            StreamSource xmlSource = new StreamSource(new StringReader(xmlString));

            // Create byteArrayOutput and create there transformed XML
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            xslTransformer.transform(xmlSource, new StreamResult(output));

            // Return output
            return output.toString();

        } catch (TransformerException e) {
            LoggerFactory.getLogger(XmlUtils.class.getName()).error("Unable to format XML with XSL.");
        }

        return null;
    }
}
