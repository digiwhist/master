package eu.dl.worker.utils.http;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Provides convenience functions for working with web.
 */
public final class URLUtils {

    private static final Logger logger = LoggerFactory.getLogger(URLUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private URLUtils() {
        throw new AssertionError();
    }

    /**
     * Gets value of some URL parameter.
     *
     * @param url
     *         tested URL
     * @param parameterName
     *         required parameter
     *
     * @return value of requested parameter or null if not found
     */
    public static String getUrlParameter(final String url, final String parameterName) {
        try {
            final List<NameValuePair> parameters = URLEncodedUtils.parse(new URI(url), "UTF-8");
            for (NameValuePair parameter : parameters) {
                if (parameter.getName().equalsIgnoreCase(parameterName)) {
                    return parameter.getValue();
                }
            }
        } catch (URISyntaxException e) {
            logger.error("Unable to get query string parameter: Invalid URL {}", url);
        }

        return null;
    }

    /**
     * Gets value of some URL parameter.
     *
     * @param url
     *         tested URL
     * @param parameterName
     *         required parameter
     *
     * @return value of requested parameter or null if not found
     */
    public static String getUrlParameter(final URL url, final String parameterName) {
        return getUrlParameter(url.toString(), parameterName);
    }

    /**
     * Appends list of parameters with their values to the url.
     *
     * @param url
     *      url
     * @param parameters
     *      list of parameters
     * @return url with apended parameters
     * @throws UnsupportedEncodingException
     *      in case that encoding of the parameter value fails
     */
    public static String addUrlParameter(final String url, final HashMap<String, Object> parameters)
        throws UnsupportedEncodingException {
        final StringBuilder urlBuilder = new StringBuilder(url);
        boolean appendAmpersand = url.contains("?");
        if (!parameters.isEmpty()) {
            for (Map.Entry<String, Object> param : parameters.entrySet()) {
                if (appendAmpersand) {
                    urlBuilder.append("&");
                } else {
                    urlBuilder.append("?");
                    appendAmpersand = true;
                }

                urlBuilder
                    .append(param.getKey())
                    .append("=")
                    .append(URLEncoder.encode(param.getValue().toString(), "UTF-8"));
            }
        }

        return urlBuilder.toString();
    }

    /**
     * Appends named parameter with given value to the url.
     *
     * @param url
     *      url
     * @param name
     *      parameter name
     * @param value
     *      parameter value
     * @return url with apended parameter
     * @throws UnsupportedEncodingException
     *      in case that encoding of the parameter value fails
     */
    public static String addUrlParameter(final String url, final String name, final Object value)
        throws UnsupportedEncodingException {
        final HashMap<String, Object> parameters = new HashMap<>();
        parameters.put(name, value);

        return addUrlParameter(url, parameters);
    }

    /**
     * Gets all URL parameters.
     *
     * @param url
     *         tested URL
     *
     * @return list of parameters with their values or null
     */
    public static HashMap<String, Object> getAllUrlParameters(final String url) {
        try {
            final List<NameValuePair> parameters = URLEncodedUtils.parse(new URI(url), "UTF-8");
            final HashMap<String, Object> allParameters = new HashMap<>();
            for (NameValuePair parameter : parameters) {
                allParameters.put(parameter.getName(), parameter.getValue());
            }
            return allParameters;
        } catch (URISyntaxException e) {
            logger.error("Unable to get query string parameters: Invalid URL {}", url);
        }

        return null;
    }
}
