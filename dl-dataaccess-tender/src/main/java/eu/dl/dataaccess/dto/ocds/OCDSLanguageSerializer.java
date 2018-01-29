package eu.dl.dataaccess.dto.ocds;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import java.io.IOException;
import org.apache.commons.lang3.StringUtils;

/**
 * Language serializer. Transform the language string to lower case.
 *
 * @author Tomas Mrazek
 */
public class OCDSLanguageSerializer extends StdSerializer<String> {
    /**
     * Default constructor.
     */
    public OCDSLanguageSerializer() {
        super(String.class);
    }

    @Override
    public final void serialize(final String value, final JsonGenerator gen, final SerializerProvider provider)
        throws IOException {
        gen.writeString(StringUtils.lowerCase(value));
    }
}
