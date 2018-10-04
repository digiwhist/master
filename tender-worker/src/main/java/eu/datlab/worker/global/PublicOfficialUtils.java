package eu.datlab.worker.global;

/**
 * Utils for Public Officials.
 */
public class PublicOfficialUtils {

    /**
     * Enum for EU countries.
     */
    public enum EuCountry {

        /**
         * Enum for EU country.
         */
        AUSTRIA(99, "Austria"),

        /**
         * Enum for EU country.
         */
        BELGIUM(106, "Belgium"),

        /**
         * Enum for EU country.
         */
        BULGARIA(55, "Bulgaria"),

        /**
         * Enum for EU country.
         */
        CYPRUS(65, "Cyprus"),

        /**
         * Enum for EU country.
         */
        CZECH_REPUBLIC(95, "Czech Republic"),

        /**
         * Enum for EU country.
         */
        DENMARK(59, "Denmark"),

        /**
         * Enum for EU country.
         */
        ESTONIA(87, "Estonia"),

        /**
         * Enum for EU country.
         */
        FINLAND(48, "Finland"),

        /**
         * Enum for EU country.
         */
        FRANCE(74, "France"),

        /**
         * Enum for EU country.
         */
        GERMANY(14, "Germany"),

        /**
         * Enum for EU country.
         */
        GREECE(68, "Greece"),

        /**
         * Enum for EU country.
         */
        HUNGARY(79, "Hungary"),

        /**
         * Enum for EU country.
         */
        IRELAND(78, "Ireland"),

        /**
         * Enum for EU country.
         */
        ITALY(108, "Italy"),

        /**
         * Enum for EU country.
         */
        LATVIA(70, "Latvia"),

        /**
         * Enum for EU country.
         */
        LITHUANIA(76, "Lithuania"),

        /**
         * Enum for EU country.
         */
        LUXEMBOURG(71, "Luxembourg"),

        /**
         * Enum for EU country.
         */
        MALTA(40, "Malta"),

        /**
         * Enum for EU country.
         */
        NETHERLANDS(15, "Netherlands"),

        /**
         * Enum for EU country.
         */
        POLAND(85, "Poland"),

        /**
         * Enum for EU country.
         */
        PORTUGAL(97, "Portugal"),

        /**
         * Enum for EU country.
         */
        ROMANIA(13, "Romania"),

        /**
         * Enum for EU country.
         */
        SLOVAKIA(38, "Slovakia"),

        /**
         * Enum for EU country.
         */
        SLOVENIA(91, "Slovenia"),

        /**
         * Enum for EU country.
         */
        SPAIN(24, "Spain"),

        /**
         * Enum for EU country.
         */
        SWEDEN(93, "Sweeden"),

        /**
         * Enum for EU country.
         */
        UNITED_KINGDOM(1, "United Kingdom");

        private final int countryNumber;
        private final String countryName;

        /**
         * Constructor for EU countries.
         *
         * @param countryNumber
         *         country ID on political data yearbook page
         * @param countryName
         *         country name in normal format
         */
        EuCountry(final int countryNumber, final String countryName) {
            this.countryNumber = countryNumber;
            this.countryName = countryName;
        }

        /**
         * Getter for property 'countryNumber'.
         *
         * @return Value for property 'countryNumber'.
         */
        public int getCountryNumber() {
            return countryNumber;
        }

        /**
         * Getter for property 'countryName'.
         *
         * @return Value for property 'countryName'.
         */
        public String getCountryName() {
            return countryName;
        }

        /**
         * Chech whether Country enum contains given string in names. Not case sensitive.
         *
         * @param stringToCheck
         *         String to check against enum names.
         *
         * @return Boolean
         */
        public static boolean contains(final String stringToCheck) {
            if (stringToCheck == null) {
                return false;
            }

            Boolean result = false;

            for (EuCountry euCountry : values()) {
                if (euCountry.getCountryName().toLowerCase().contains(stringToCheck.toLowerCase())) {
                    result = true;
                }
            }
            return result;
        }
    }
}
