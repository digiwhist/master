package eu.dl.dataaccess.dao.mongo;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;

import org.mongojack.internal.DateDeserializer;
import org.mongojack.internal.DateSerializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.deser.std.NumberDeserializers;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.NumberSerializers;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

/**
 *
 */
public class MongoJacksonModule extends SimpleModule {

    /**
     *
     */
    private static final long serialVersionUID = 2618344528969921731L;

    /**
     * Initializes new instance with custom JSON serializers and deserializers useful for mapping to and from MongoDb
     * (eg. BSON date).
     */
    protected MongoJacksonModule() {
        addSerializer(LocalDateTime.class, new MongoLocalDateTimeSerializer());
        addSerializer(LocalDate.class, new MongoLocalDateSerializer());
        addSerializer(BigDecimal.class, new MongoBigDecimalSerializer());

        addDeserializer(LocalDateTime.class, new MongoLocalDateTimeDeserializer());
        addDeserializer(LocalDate.class, new MongoLocalDateDeserializer());
        addDeserializer(BigDecimal.class, new MongoBigDecimalDeserializer());
    }

    /**
     * Serializer for {@link LocalDateTime} to BSON dates.
     */
    private final class MongoLocalDateTimeSerializer extends StdSerializer<LocalDateTime> {
        /**
         *
         */
        private static final long serialVersionUID = 7544295730084728586L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoLocalDateTimeSerializer() {
            super(LocalDateTime.class);
        }

        @Override
        public void serialize(final LocalDateTime localDateTime, final JsonGenerator jsonGenerator,
                final SerializerProvider serializerProvider) throws IOException, JsonProcessingException {
            final Instant instant = localDateTime.toInstant(ZoneOffset.UTC);
            final Date date = Date.from(instant);
            new DateSerializer().serialize(date, jsonGenerator, serializerProvider);
        }
    }

    /**
     * Deserializer for {@link LocalDateTime} from BSON dates.
     */
    private final class MongoLocalDateTimeDeserializer extends StdDeserializer<LocalDateTime> {
        /**
         *
         */
        private static final long serialVersionUID = 4217948674548325049L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoLocalDateTimeDeserializer() {
            super(LocalDateTime.class);
        }

        @Override
        public LocalDateTime deserialize(final JsonParser jsonParser,
                final DeserializationContext deserializationContext) throws IOException, JsonProcessingException {
            final Date date = new DateDeserializer().deserialize(jsonParser, deserializationContext);
            final Instant instant = date.toInstant();
            return LocalDateTime.ofInstant(instant, ZoneOffset.UTC);
        }
    }

    /**
     * Serializer for {@link LocalDate} to BSON dates.
     */
    private final class MongoLocalDateSerializer extends StdSerializer<LocalDate> {
        /**
         *
         */
        private static final long serialVersionUID = 5385530879700064628L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoLocalDateSerializer() {
            super(LocalDate.class);
        }

        @Override
        public void serialize(final LocalDate localDate, final JsonGenerator jsonGenerator,
                final SerializerProvider serializerProvider) throws IOException, JsonProcessingException {
            final Instant instant = localDate.atStartOfDay().toInstant(ZoneOffset.UTC);
            final Date date = Date.from(instant);
            new DateSerializer().serialize(date, jsonGenerator, serializerProvider);
        }
    }

    /**
     * Deserializer for {@link LocalDateTime} from BSON dates.
     */
    private final class MongoLocalDateDeserializer extends StdDeserializer<LocalDate> {
        /**
         *
         */
        private static final long serialVersionUID = -9098466860639932838L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoLocalDateDeserializer() {
            super(LocalDate.class);
        }

        @Override
        public LocalDate deserialize(final JsonParser jsonParser,
                final DeserializationContext deserializationContext) throws IOException, JsonProcessingException {
            final Date date = new DateDeserializer().deserialize(jsonParser, deserializationContext);
            final Instant instant = date.toInstant();
            return LocalDateTime.ofInstant(instant, ZoneOffset.UTC).toLocalDate();
        }
    }

    /**
     * Serializer for {@link BigDecimal} to BSON double.
     */
    private final class MongoBigDecimalSerializer extends StdSerializer<BigDecimal> {
        /**
         *
         */
        private static final long serialVersionUID = -8232158788593075843L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoBigDecimalSerializer() {
            super(BigDecimal.class);
        }

        @Override
        public void serialize(final BigDecimal bigDecimal, final JsonGenerator jsonGenerator,
                final SerializerProvider serializerProvider) throws IOException, JsonProcessingException {

            new NumberSerializers.DoubleSerializer(Double.class).serialize(bigDecimal.doubleValue(), jsonGenerator,
                    serializerProvider);
        }
    }

    /**
     * Deserializer for {@link BigDecimal} from BSON double.
     */
    private final class MongoBigDecimalDeserializer extends StdDeserializer<BigDecimal> {

        /**
         *
         */
        private static final long serialVersionUID = 732922192432806126L;

        /**
         * Initializes deserializer by calling superclass constructor.
         */
        protected MongoBigDecimalDeserializer() {
            super(BigDecimal.class);
        }

        @Override
        public BigDecimal deserialize(final JsonParser jsonParser,
                final DeserializationContext deserializationContext) throws IOException, JsonProcessingException {

            final Double doubleValue = new NumberDeserializers.DoubleDeserializer(Double.class, Double.NaN).deserialize(
                    jsonParser, deserializationContext);

            return (doubleValue.equals(Double.NaN) ? null : new BigDecimal(doubleValue));
        }
    }
}
