package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import java.util.List;
import java.util.Map;


/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class AddressUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private AddressUtils() {

    }

    /**
     * Cleans the given parsed address.
     *
     * @param parsedAddress
     *         parsed address
     *
     * @return cleaned address
     */
    public static Address cleanAddress(final ParsedAddress parsedAddress) {
        if (parsedAddress == null) {
            return null;
        }

        return new Address()
                .setCity(StringUtils.cleanShortString(parsedAddress.getCity()))
                .setState(StringUtils.cleanShortString(parsedAddress.getState()))
                .setCountry(StringUtils.cleanShortString(parsedAddress.getCountry()))
                .setNuts(ArrayUtils.walk(parsedAddress.getNuts(),
                        (parsedNuts) -> StringUtils.cleanShortString(parsedNuts)))
                .setPostcode(StringUtils.cleanShortString(parsedAddress.getPostcode()))
                .setRawAddress(StringUtils.cleanLongString(parsedAddress.getRawAddress()))
                .setStreet(StringUtils.cleanShortString(parsedAddress.getStreet()))
                .setUrl(StringUtils.cleanURL(parsedAddress.getUrl(), URLSchemeType.HTTP));
    }

    /**
     * Cleans the given parsed address.
     *
     * @param parsedAddress
     *         parsed address
     * @param countryMapping
     *         country mapping
     *
     * @return cleaned address
     */
    public static Address cleanAddress(final ParsedAddress parsedAddress, final Map<Enum, List<String>>
            countryMapping) {
        final Address cleanAddress = cleanAddress(parsedAddress);

        if (cleanAddress == null || cleanAddress.getCountry() == null || countryMapping == null) {
            return cleanAddress;
        } else {
            Enum country = CodeTableUtils.mapValue(cleanAddress.getCountry(), countryMapping);
            return cleanAddress.setCountry(country != null ? country.toString() : null);
        }
    }
}
