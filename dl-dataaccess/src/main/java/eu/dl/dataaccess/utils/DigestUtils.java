package eu.dl.dataaccess.utils;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Class provides string standardization functions, hash function etc.
 * 
 * @author Tomas Mrazek
 */
public final class DigestUtils {
    
    private static final String SPACE_REGEX = "[,\\.\\s]";

    /**
     * Set of regular expressions and their replacements which are used for replacing/standardizing of company types
     * within company name.
     */
    public static final HashMap<String, Map<String, String>> COMPANY_TYPE_REGEX_REPLACEMENT = new HashMap<>();
    static {
        // CZECH + SLOVAKIA
        HashMap<String, String> repl = new HashMap<>();
        repl.put("AS", "(a|akc|akciová) (s|spol|společnost|spoločnosť)");
        repl.put("SR", "(spol|společnost|spoločnosť)? s (r|ručením) (o|omezeným|obmedzeným)");
        repl.put("ZS", "o s, z s");
        repl.put("OP", "o p s");
        repl.put("VO", "v o s");
        repl.put("SP", "s p, státní podnik, štátny podnik");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("CZ", repl);
        COMPANY_TYPE_REGEX_REPLACEMENT.put("SK", repl);

        //FR
        repl = new HashMap<>();
        repl.put("SL", "S A R L");
        repl.put("SZ", "S A S U");
        repl.put("SS", "S A S");
        repl.put("SE", "Société anonyme d économie mixte, S E M");
        repl.put("SA", "S A");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("FR", repl);

        //UK, IE
        repl = new HashMap<>();
        repl.put("LL", "L L P, Limited liability partnership");
        repl.put("LT", "(Single private|co) L T D, limited");
        repl.put("PL", "p l c, public limited company");
        repl.put("LP", "l p, Limited partnership");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("UK", repl);
        COMPANY_TYPE_REGEX_REPLACEMENT.put("IE", repl);

        //DE
        repl = new HashMap<>();
        repl.put("GM", "g m b h");
        repl.put("AG", "A G");
        repl.put("KG", "(&)? (co)? K G");
        // (be greedy - gmbh has priority!)
        repl.put("MB", "m b h");
        repl.put("F", "fa, firma");
        repl.put("B", "BIETERGEMEINSCHAFT");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("DE", repl);

        //ES, PT
        repl = new HashMap<>();
        repl.put("SU", "S A U");
        repl.put("SA", "S A");
        // (be greedy take longer patterns first)
        repl.put("SN", "S L N E, S L (;|:)?, Sociedad Limitada Nueva Empresa, Sociedad Limitada");
        repl.put("UT", "U T E, Unión Temporal de Empresas");
        repl.put("SV", "S L U");
        repl.put("LD", "L D A, L D");
        repl.put("EP", "E P E");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("ES", repl);
        COMPANY_TYPE_REGEX_REPLACEMENT.put("PT", repl);

        //PL
        repl = new HashMap<>();
        repl.put("ZO", "(sp)? z o o (sp)? (k)?, Spółka z ograniczoną odpowiedzialnością");
        repl.put("PR", "PRZEDSIĘBIORSTWO");
        repl.put("SJ", "sp j");
        repl.put("SA", "S A");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("PL", repl);

        //IT
        repl = new HashMap<>();
        repl.put("SI", "S R L");
        repl.put("SY", "S P A");
        repl.put("RT", "R T I");
        repl.put("AT", "A T I");
        repl.put("AR", "A R L");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("IT", repl);

        //NL
        repl = new HashMap<>();
        repl.put("BV", "B V");
        repl.put("NV", "N V");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("NL", repl);

        //SE
        repl = new HashMap<>();
        repl.put("AB", "A B, AKTIEBOLAG, AKTIEBOLAGET");
        repl.put("HB", "H B");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("SE", repl);

        //HU
        repl = new HashMap<>();
        repl.put("KF", "K F T");
        repl.put("ZR", "Z R T");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("HU", repl);

        //DE
        repl = new HashMap<>();
        repl.put("NV", "N V");
        COMPANY_TYPE_REGEX_REPLACEMENT.put("BE", repl);
    }
    
    /**
     * Separator used when creating digest.
     */
    public static final String SEPARATOR = "|";

    /**
     * Separator used when creating complex body hash.
     */
    public static final String FULL_BODY_HASH_SEPARATOR = "|";
    
    /**
     * Separator used when creating body hash.
     */
    public static final String HASH_SEPARATOR = "^";
    
    /**
     * String value used instead of NULL when creating complex body hash.
     */
    public static final String HASH_NULL = "";
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private DigestUtils() {
    }

    /**
     * Standardizes the given string.
     * 
     * @param input
     *      string that needs stadardize
     * @return standardized string
     */
    public static String standardize(final String input) {
        if (input == null) {
            return null;
        }

        return input
            .trim()
            .toLowerCase()            
            .replaceAll("\\s+", " ");
    }
    
    /**
     * Standardizes body name. Appends additional adjustments of the name to common standardizition via
     * {@code DigestUtils#standardize(java.lang.String)} function.
     * 
     * @see DigestUtils#standardize(java.lang.String) 
     * 
     * @param name
     *      name of the body
     * @return standardized name
     */
    public static String standardizeName(final String name) {
        if (name == null) {
            return null;
        }

        String stdName = standardize(name);

        Map<String, String> regex = new HashMap();
        // merging of regexes for selected countries
        COMPANY_TYPE_REGEX_REPLACEMENT.entrySet().stream()
            // TODO - filtering by country(ies)
            //.filter()
            .forEach(n -> regex.putAll(n.getValue()));
        
        stdName = replace(stdName, regex);

        return StringUtils.substring(removeAccents(stdName), 0, 513);
    }

    /**
     * Replaces the pseudo {@code regex} with {@code replacement} in the given {@code input}.
     *
     * @param input
     *      string to be replaced
     * @param regex
     *      comma separated regexes for replacing. Note that these are sorted by length in descending order before
     *      replacing.
     * @param replacement
     *      replacement
     * @return replaced string
     */
    public static String replace(final String input, final String regex, final String replacement) {
        if (input == null || regex == null || replacement == null) {
            return input;
        }
        
        String cleanInput = input;
        List<String> sortedRegex = parseExpressions(regex).stream()
            .sorted(sortDescByLength(o -> o.length())).collect(Collectors.toList());

        for (String r : sortedRegex) {
            String exp =
                // begin of the input
               "(?i)(\\A?" + SPACE_REGEX + "+|\\A|(?<before>\\|))"
               // matched regex
               + r.replace(" ", SPACE_REGEX + "*")
               // end of the input
               + "(" + SPACE_REGEX + "+\\z?|\\z|(?<after>\\|))";
                        
            cleanInput = cleanInput.replaceAll(exp, "${before}|" + replacement + "|${after}");
        }

        return cleanInput;
    }

    /**
     * Apply all matched replacements. Regular expresions are sorted by length in descending order and duplicities are
     * removed before replacing.
     *
     * @param input
     *      string to be replaced
     * @param regex
     *      mapping of regular expresions, where replacement is key and regular expression is value
     * @return replaced string or input if no regex matches
     */
    public static String replace(final String input, final Map<String, String> regex) {
        if (input == null || regex == null) {
            return input;
        }

        final List<Pair<String, String>> expressions = new ArrayList<>();
        regex.entrySet().stream()
            // unique list by key (replacement), assumes that same keys includes same regular expression so we can
            // remove duplicities
            .filter(distinct(n -> n.getKey()))
            // parse all regular expressions, one item may includes comma separated expressions
            .forEach(n -> {
                parseExpressions(n.getValue()).forEach(m -> expressions.add(Pair.of(n.getKey(), m)));
            });

        // expressions sorting and replacing
        final String[] cleanInput = {input};
        expressions.stream()
            .sorted(sortDescByLength(o -> o.getValue().length()))
            .forEach(n -> {
               cleanInput[0] = replace(cleanInput[0], n.getValue(), n.getKey());
            });
                
        return cleanInput[0];
    }

    /**
     * Distinct keys in stream.
     *
     * @param keyExtractor
     *      key extractor
     * @param <T>
     *      list item class
     * @return distinct result
     */
    private static <T> Predicate<T> distinct(final Function<? super T, ?> keyExtractor) {
        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    /**
     * Sorting by length in descending order.
     *
     * @param <T>
     *      compared items class
     * @param lengthExtractor
     *      item length extractor
     * @return length comparator
     */
    private static <T> Comparator<T> sortDescByLength(final Function<T, Integer> lengthExtractor) {
        return new Comparator<T>() {
            @Override
            public int compare(final T o1, final T o2) {
                return Integer.compare(lengthExtractor.apply(o2), lengthExtractor.apply(o1));
            }
        };
    }

    /**
     * @param regex
     *      comma separated regular expresions
     * @return list of regular expressions or an empty list
     */
    private static List<String> parseExpressions(final String regex) {
        if (regex == null) {
            return Collections.emptyList();
        }
        
        return Arrays.asList(regex.split(", ?")).stream()
            .collect(Collectors.toList());
    }
    
    /**
     * Standardizes body address as concatenation of standardized values of particular fields.
     * 
     * @see DigestUtils#standardize(java.lang.String) 
     * 
     * @param address
     *      address of the body
     * @return standardized address
     */
    public static String standardizeAddress(final Address address) {
        if (address == null) {
            return null;
        }
        
        final StringBuilder sb = new StringBuilder();        
        if (address.getStreet() != null && address.getCity() != null) {
            sb
                .append(standardize(address.getStreet()))
                .append(" ")
                .append(standardize(address.getCity()));                
        } else if (address.getRawAddress() != null) {
            sb
                .append(standardize(address.getRawAddress()));
        }
        
        //appends country in case that buffered string length is greater than 0 and country exists
        if (sb.length() > 0 && address.getCountry() != null) {
            sb
                .append(" ")
                .append(standardize(address.getCountry()));
        }
        
        return (sb.length() == 0 ? null : StringUtils.substring(removeAccents(sb.toString()), 0, 513));
    }
    
    /**
     * Generates hash for the given clean body.
     *
     * @param body
     *      matched body
     * @return body hash
     */
    public static String bodyHash(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        List<BodyIdentifier> bodyIds = Collections.emptyList();
        if (body.getBodyIds() != null) {
            bodyIds = body.getBodyIds().stream()
                //Only bodyIds that aren't ETALON_ID and such that have non-null scope and type.
                .filter(bodyId -> bodyId.getScope() != null
                    && !Objects.equals(bodyId.getType(), BodyIdentifier.Type.ETALON_ID))
                .filter(distinct(n -> n.getScope().name() + standardize(n.getId())))
                //Sort by scope and id
                .sorted(sortBodyIdsForHash())
                .collect(Collectors.toList());
        }

        if (body.getName() != null && !bodyIds.isEmpty()) {
            final StringBuilder sb = new StringBuilder();
            
            sb.append("name").append(standardizeName(body.getName()));
            
            bodyIds.forEach((bodyId) -> sb
                .append(bodyId.getScope().name())
                .append(standardize(bodyId.getId())));
            
            return sha256Hex(sb.toString());
        } else {
            return sha256Hex(UUID.randomUUID().toString());
        }
    }

    /**
     * @return comparator for body ids sorting by scope and id
     */
    private static Comparator<BodyIdentifier> sortBodyIdsForHash() {
        return (a, b) -> {
            return (a.getScope().name() + a.getId())
                    .compareToIgnoreCase(b.getScope().name() + b.getId());
        };
    }

    /**
     * Returns first three alphanumerical symbols in a standardized name (turned into lowercase).
     * 
     * @param stadardizedName
     *      standardized name that needs digest
     * @return digest of the name or null
     */
    public static String digestName(final String stadardizedName) {
        if (stadardizedName == null) {
            return null;
        }

        //matches arbitrary alphanumerical character
        final String digest = digestString(stadardizedName.toLowerCase(), "[\\p{L}\\d]", 3);
                
        return (digest.length() == 3 ? digest : null);
    }
    
    /**
     * Returns first two alphabetical symbols and a first number (regardless of a number of digits) in standardized
     * address. In case, that the street includes no number use next two alphabetical symbols instead.
     * 
     * @param standardizeAddress
     *      standardized address that needs digest
     * @return digest of the street address
     */
    public static String digestAddress(final String standardizeAddress) {
        if (standardizeAddress == null) {
            return null;
        }
    
        final String numberDigest = digestString(standardizeAddress.toLowerCase(), "[0-9]+", 1);
        
        final String alphabetDigest = digestString(standardizeAddress.toLowerCase(), "[\\p{L}]",
            numberDigest.isEmpty() ? 4 : 2);

        final String digest = alphabetDigest + numberDigest;
        
        return (digest.length() >= 3 ? digest : null);
    }

    /**
     * Returns digest of the given {@code input} as a concatenation of
     * substrings that matches the {@code regex}. Joins maximal {@code count} of
     * substrings.
     * 
     * @param input
     *            digested string
     * @param regex
     *            regular expression of substring
     * @param count
     *            maximal count of concatenated substrings
     * @return non-empty digest of an input. In case that for some reason isn't
     *         able to build digest string returns an empty string.
     */
    public static String digestString(final String input, final String regex, final int count) {
        if (input == null || regex == null || count <= 0) {
            return "";
        }
        
        final StringBuilder digest = new StringBuilder();
        final Pattern p = Pattern.compile(regex);
        final Matcher m = p.matcher(input);        
        while (m.find() && digest.length() < count) {
            digest.append(m.group());
        }
        
        return digest.toString();
    }
    
    /**
     * Removes accents from {@code input} string.
     *
     * @param input
     *      string that potencialy includes accended characters
     * @return {@code input} string without accents
     */
    public static String removeAccents(final String input) {
        if (input == null) {
            return null;
        }

        return
            //input canonical decomposition
            Normalizer.normalize(input, Normalizer.Form.NFD)
            //accents removing
            .replaceAll("[\\p{InCombiningDiacriticalMarks}]", "");
    }

    /**
     * Returns combination of digests for standardized name and address. Both values are separated by a
     * DigestUtils.SEPARATOR.
     * 
     * @see #digestName(java.lang.String)
     * @see #digestAddress(java.lang.String)
     * 
     * @param standardizedName
     *      standardized name that needs digest
     * @param standardizedAddress
     *      standardized address that needs digest
     * @return combined digest such a name_digestSEPARATORaddress_digest
     */
    public static String digest(final String standardizedName, final String standardizedAddress) {
        String digName = digestName(standardizedName);
        String digAddr = digestAddress(standardizedAddress);
        
        StringBuilder sb = new StringBuilder();
        if (digName != null) {
            sb.append(digName);
        }
        
        sb.append(SEPARATOR);
        
        if (digAddr != null) {
            sb.append(digAddr);
        }

        return (sb.length() == 0 || sb.toString().equals(SEPARATOR)) ? null : sb.toString();
    }

    /**
     * Returns combination of digests for the given matched {@code body}. In case that standardized address is null for
     * the address of this body, attempts to compose raw address and uses it's stadardized form in digest. Raw address
     * must includes at least two not null parameters from following four street, city, postcode, and country.
     *
     * @see #digest(java.lang.String, java.lang.String)
     * @see #standardizeAddress(eu.dl.dataaccess.dto.generic.Address)
     *
     * @param body
     *      matched body
     * @return combined digest such a name_digestSEPARATORaddress_digest
     */
    public static String digest(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        Address adr = body.getAddress();
        String stdName = standardizeName(body.getName());
        String digest = digest(stdName, standardizeAddress(adr));

        if ((digest == null || digest.endsWith("|")) && adr != null) {
            List<String> fields = Arrays.asList(adr.getStreet(), adr.getCity(), adr.getPostcode(), adr.getCountry());
            
            if (fields.stream().mapToInt(n -> n == null ? 0 : 1).sum() >= 2) {
                String rawAddr = fields.stream().reduce("", (r, n) -> r + Optional.ofNullable(n).orElse(""));

                return digest(stdName, standardizeAddress(new Address().setRawAddress(rawAddr)));
            }
        }

        return digest;
    }

    /**
     * Generates complex hash for the given clean body.
     *
     * @param body
     *      matched body
     * @return body hash
     */
    public static String bodyFullHash(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        Optional<Address> addr = Optional.ofNullable(body.getAddress());

        StringBuilder sb = new StringBuilder()
            .append(Optional.ofNullable(body.getName()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getStreet()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getCity()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getCountry()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getState()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getRawAddress()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getPostcode()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(addr.map(a -> a.getUrl()).map(u -> u.toString()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            // alphabetically ascend sorted NUTS codes
            .append(String.join(FULL_BODY_HASH_SEPARATOR, addr.map(a -> a.getNuts()).orElse(Collections.emptyList())
                .stream().sorted().collect(Collectors.toList()))).append(FULL_BODY_HASH_SEPARATOR)
            // alphabetically ascend sorted non-etalon body identifiers ids
            .append(String.join(FULL_BODY_HASH_SEPARATOR,
                // get body ids if is presented or empty list
                Optional.ofNullable(body.getBodyIds()).orElse(Collections.emptyList())
                // use only non ETALON_ID identifiers and sort them alphabetically
                .stream().filter(n -> n.getType() != BodyIdentifier.Type.ETALON_ID).map(n -> n.getId())
                .sorted().collect(Collectors.toList()))).append(FULL_BODY_HASH_SEPARATOR)
            .append(Optional.ofNullable(body.getEmail()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(Optional.ofNullable(body.getContactPoint()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(Optional.ofNullable(body.getContactName()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(Optional.ofNullable(body.getPhone()).orElse(HASH_NULL)).append(FULL_BODY_HASH_SEPARATOR)
            .append(Optional.ofNullable(body.getBuyerType()).map(n -> n.name()).orElse(HASH_NULL));

        return sha256Hex(sb.toString().toLowerCase());
    }
    
    /**
     * Generates hash in form standardized_name|standardized_address.
     * @param body matched body
     * @return hash or null if hash cannot be generated
     */
    public static WeightedHash nameAddressHash(final MatchedBody body) {
    		String stdName = standardizeName(body.getName());
    		String stdAddress = standardizeAddress(body.getAddress());
    		
    		if (stdName != null && stdAddress != null) {
    			return new WeightedHash()
    					.setHash(sha256Hex(stdName + HASH_SEPARATOR + stdAddress))
    					.setWeight(2.2);
    		}
    		
    		return null;
    }
    
    /**
     * Generates hash in form standardized_name|standardized_address.
     * @param body matched body
     * @return hash or null if hash cannot be generated
     */
    public static List<WeightedHash> nameAddressIdHash(final MatchedBody body) {
    		String stdName = standardizeName(body.getName());
    		String stdAddress = standardizeAddress(body.getAddress());
    		List<WeightedHash> result = new ArrayList<WeightedHash>();
    		if (stdName != null && stdAddress != null && body.getBodyIds() != null && !body.getBodyIds().isEmpty()) {
    			for (BodyIdentifier bodyId : body.getBodyIds()) {
	    			WeightedHash hash = new WeightedHash();
	    			hash.setHash(sha256Hex(stdName + HASH_SEPARATOR + stdAddress + HASH_SEPARATOR 
	    					+ bodyId.getScope() + HASH_SEPARATOR + bodyId.getId()));
	    			hash.setWeight(3.0);
	    			result.add(hash);
	    		}
    		}
    		
    		if (stdName != null && body.getBodyIds() != null && !body.getBodyIds().isEmpty()) {
    			for (BodyIdentifier bodyId : body.getBodyIds()) {
	    			WeightedHash hash = new WeightedHash();
	    			hash.setHash(sha256Hex(stdName + HASH_SEPARATOR 
	    					+ bodyId.getScope() + HASH_SEPARATOR + bodyId.getId()));
	    			hash.setWeight(2.3);
	    			result.add(hash);
	    		}
    		}
    		
    		if (stdAddress != null && body.getBodyIds() != null && !body.getBodyIds().isEmpty()) {
    			for (BodyIdentifier bodyId : body.getBodyIds()) {
	    			WeightedHash hash = new WeightedHash();
	    			hash.setHash(sha256Hex(stdAddress + HASH_SEPARATOR 
	    					+ bodyId.getScope() + HASH_SEPARATOR + bodyId.getId()));
	    			hash.setWeight(2.1);
	    			result.add(hash);
	    		}
    		}
    		
    		if (!result.isEmpty()) {
				return result;
		} 
    		
    		return null;
    }
    
    /**
     * Generates hash list relevant for given body. The hashes should be ordered by its priority.
     * 
     * @param body to generate hases for
     * @return generated hashes
     */
    public static List<WeightedHash> generateAlternativeBodyHashes(final MatchedBody body) {
		List<WeightedHash> hashes = new ArrayList<WeightedHash>();
		List<WeightedHash> nameAddressIdHashes = nameAddressIdHash(body);
		if (nameAddressIdHashes != null) {
			hashes.addAll(nameAddressIdHash(body));
		}
		
		WeightedHash nameAddressHash = nameAddressHash(body);
		if (nameAddressHash != null) {
			hashes.add(nameAddressHash);
		}
		
    		return hashes;
	}
}
