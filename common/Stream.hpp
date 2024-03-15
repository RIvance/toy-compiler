#ifndef STREAM_HPP
#define STREAM_HPP

#include "Common.inc"

inline namespace stream
{
    template <Iterable TCollection>
    struct Collection
    {
        using Value = typename TCollection::value_type;
        template <typename R>
        using WithValueType = Vector<R>;

        template <typename R>
        static void insert(Vector<Value>& collection, R&& value) {
            collection.emplace_back(std::forward<R>(value));
        }
    };

    template <typename T>
    struct Collection<HashSet<T>>
    {
        using Value = typename HashSet<T>::value_type;
        template <typename R>
        using WithValueType = HashSet<R>;

        static void insert(HashSet<Value>& collection, auto value) {
            collection.insert(value);
        }
    };

    template <typename T>
    struct Collection<TreeSet<T>>
    {
        using Value = typename TreeSet<T>::value_type;
        template <typename R>
        using WithValueType = TreeSet<R>;

        static void insert(TreeSet<Value>& collection, auto value) {
            collection.insert(value);
        }
    };

    template <Iterable TCollection>
    class Stream;

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        typename R, Mapper<typename TStream::Value, R> FMapper>
    class Map;

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    class Filter;

    template <Iterable TCollection>
    struct Take;

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    struct TakeWhile;

    template <Iterable TCollection>
    struct Skip;

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    struct SkipWhile;

    template <Iterable TCollection>
    class Stream
    {
      protected:

        using Iterator = typename TCollection::const_iterator;

        Iterator begin;
        Iterator end;

        Stream() = default;

      public:

        using Value = typename TCollection::value_type;

        explicit Stream(TCollection& collection) : begin(collection.begin()), end(collection.end()) {}

        template <typename R, Mapper<Value, R> FMapper>
        auto map(FMapper mapper) -> Map<TCollection, Stream, R, FMapper> {
            return Map<TCollection, Stream, R, FMapper>(mapper, begin, end);
        }

        template <Predicate<Value> FPredicate>
        auto filter(FPredicate predicate) -> Filter<TCollection, Stream, FPredicate> {
            return Filter<TCollection, Stream, FPredicate>(predicate, begin, end);
        }

        auto take(usize count) -> Take<TCollection> {
            return Take<TCollection>(count, begin, end);
        }

        template <Predicate<Value> FPredicate>
        auto takeWhile(FPredicate predicate) -> TakeWhile<TCollection, Stream, FPredicate> {
            return TakeWhile<TCollection, Stream, FPredicate>(predicate, begin, end);
        }

        auto skip(usize count) -> Skip<TCollection> {
            return Skip<TCollection>(count, begin, end);
        }

        template <Predicate<Value> FPredicate>
        auto skipWhile(FPredicate predicate) -> SkipWhile<TCollection, Stream, FPredicate> {
            return SkipWhile<TCollection, Stream, FPredicate>(predicate, begin, end);
        }

        template <Consumer<const Value&> FConsumer>
        void forEach(FConsumer consumer) {
            for (auto iter = begin; iter != end; ++iter) {
                consumer(*iter);
            }
        }

        template <KeyValueConsumer<usize, const Value&> FConsumer>
        void forEachIndexed(FConsumer consumer) {
            usize index = 0;
            for (auto iter = begin; iter != end; ++iter) {
                consumer(index++, *iter);
            }
        }

        template <Reducer<Value, Value> FReducer>
        Value reduce(FReducer reducer) {
            auto iter = begin;
            Value acc = *iter++;
            for (; iter != end; ++iter) {
                acc = reducer(acc, *iter);
            }
            return acc;
        }

        template <typename R, Reducer<Value, R> FReducer>
        R reduce(R init, FReducer reducer) {
            R result = init;
            for (auto iter = begin; iter != end; ++iter) {
                result = reducer(result, *iter);
            }
            return result;
        }

        template <Predicate<Value> FPredicate>
        bool any(FPredicate predicate) {
            for (auto iter = begin; iter != end; ++iter) {
                if (predicate(*iter)) { return true; }
            }
            return false;
        }

        template <Predicate<Value> FPredicate>
        bool all(FPredicate predicate) {
            for (auto iter = begin; iter != end; ++iter) {
                if (!predicate(*iter)) { return false; }
            }
            return true;
        }
    };

    template <Iterable TCollection>
    class MutStream : public Stream<TCollection>
    {
      protected:

        using MutIterator = typename TCollection::iterator;

        MutIterator mutBegin;
        MutIterator mutEnd;

      public:

        template <typename RCollection>
        RCollection collect() {
            RCollection result;
            for (auto iter = mutBegin; iter != mutEnd; ++iter) {
                Collection<RCollection>::insert(result, std::move(*iter));
            }
            return result;
        }
    };

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        typename R, Mapper<typename TStream::Value, R> FMapper>
    class Map final : public MutStream<typename Collection<TCollection>::template WithValueType<R>>
    {
      private:

        using RCollection = typename Collection<TCollection>::template WithValueType<R>;

        RCollection mapped;

        using TIterator = typename TCollection::const_iterator;

        static RCollection map(FMapper mapper, const TIterator& begin, const TIterator& end) {
            RCollection mapped;
            for (auto iter = begin; iter != end; ++iter) {
                const typename TStream::Value& value = *iter;
                Collection<RCollection>::insert(mapped, mapper(value));
            }
            return mapped;
        }

      public:

        explicit Map(
            FMapper mapper, const TIterator& begin, const TIterator& end
        ) : MutStream<RCollection>(), mapped(map(mapper, begin, end)) {
            this->begin = mapped.begin();
            this->end = mapped.end();
            this->mutBegin = mapped.begin();
            this->mutEnd = mapped.end();
        }
    };

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    class Filter final : public MutStream<TCollection>
    {
      private:

        TCollection filtered;

        static TCollection filter(
            FPredicate predicate, const typename Filter::Iterator& begin, const typename Filter::Iterator& end
        ) {
            std::remove_const_t<TCollection> filtered;
            for (auto iter = begin; iter != end; ++iter) {
                if (predicate(*iter)) { Collection<TCollection>::insert(filtered, *iter); }
            }
            return filtered;
        }

      public:

        explicit Filter(
            FPredicate predicate, const typename Filter::Iterator& begin, const typename Filter::Iterator& end
        ) : MutStream<TCollection>(), filtered(filter(predicate, begin, end)) {
            this->begin = filtered.begin();
            this->end = filtered.end();
        }
    };

    template <Iterable TCollection>
    struct Take final : MutStream<TCollection>
    {
        explicit Take(
            usize count, const typename Take::Iterator& begin, const typename Take::Iterator& end
        ) : MutStream<TCollection>() {
            this->begin = begin;
            auto iter = begin;
            for (usize i = 0; i < count && iter != end; ++i) { ++iter; }
            this->end = iter;
        }
    };

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    struct TakeWhile final : MutStream<TCollection>
    {
        explicit TakeWhile(
            FPredicate predicate, const typename TakeWhile::Iterator& begin, const typename TakeWhile::Iterator& end
        ) : MutStream<TCollection>() {
            this->begin = begin;
            for (auto iter = begin; iter != end; ++iter) {
                if (!predicate(*iter)) {
                    this->end = iter;
                    return;
                }
            }
            this->end = end;
        }
    };

    template <Iterable TCollection>
    struct Skip final : Stream<TCollection>
    {
        explicit Skip(
            usize count, const typename Skip::Iterator& begin, const typename Skip::Iterator& end
        ) : Stream<TCollection>() {
            this->end = end;
            auto iter = begin;
            for (usize i = 0; i < count && iter != end; ++i) { ++iter; }
            this->begin = iter;
        }
    };

    template <
        Iterable TCollection, Derives<Stream<TCollection>> TStream,
        Predicate<typename TStream::Value> FPredicate>
    struct SkipWhile final : Stream<TCollection>
    {
        explicit SkipWhile(
            FPredicate predicate, const typename SkipWhile::Iterator& begin, const typename SkipWhile::Iterator& end
        ) : Stream<TCollection>() {
            this->end = end;
            for (auto iter = begin; iter != end; ++iter) {
                if (!predicate(*iter)) {
                    this->begin = iter;
                    return;
                }
            }
            this->begin = end;
        }
    };
}

#endif // STREAM_HPP
