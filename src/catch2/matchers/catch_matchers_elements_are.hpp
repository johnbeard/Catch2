
//              Copyright Catch2 Authors
// Distributed under the Boost Software License, Version 1.0.
//   (See accompanying file LICENSE_1_0.txt or copy at
//        https://www.boost.org/LICENSE_1_0.txt)

// SPDX-License-Identifier: BSL-1.0
#ifndef CATCH_MATCHERS_ELEMENTS_ARE_HPP_INCLUDED
#define CATCH_MATCHERS_ELEMENTS_ARE_HPP_INCLUDED

#include <algorithm>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <utility>
#include <memory>

namespace Catch {
    namespace Matchers {

        /**
         * Matcher for checking that an element contains the same
         * elements in the same order
         */
        template <typename TargetRangeLike, typename Equality>
        class ElementsAreElementsMatcher final : public MatcherGenericBase {
            TargetRangeLike m_desired;
            Equality m_predicate;

        public:
            template <typename TargetRangeLike2, typename Equality2>
            ElementsAreElementsMatcher( TargetRangeLike2&& range,
                                        Equality2&& predicate ):
                m_desired( CATCH_FORWARD( range ) ),
                m_predicate( CATCH_FORWARD( predicate ) ) {}

            template <typename RangeLike> bool match( RangeLike&& rng ) const {
                return std::equal( begin( m_desired ),
                                   end( m_desired ),
                                   begin( rng ),
                                   end( rng ),
                                   m_predicate );
            }

            std::string describe() const override {
                return "elements are " + Catch::Detail::stringify( m_desired );
            }
        };

        /**
         * Matcher for checking that an element contains the same
         * elements (but not necessarily in the same order)
         */
        template <typename TargetRangeLike, typename Equality>
        class UnorderedElementsAreElementsMatcher final
            : public MatcherGenericBase {
            TargetRangeLike m_desired;
            Equality m_predicate;

        public:
            template <typename TargetRangeLike2, typename Equality2>
            UnorderedElementsAreElementsMatcher( TargetRangeLike2&& range,
                                                 Equality2&& predicate ):
                m_desired( CATCH_FORWARD( range ) ),
                m_predicate( CATCH_FORWARD( predicate ) ) {}

            template <typename RangeLike> bool match( RangeLike&& rng ) const {
                return std::is_permutation( begin( m_desired ),
                                            end( m_desired ),
                                            begin( rng ),
                                            end(rng ),
                                            m_predicate );
            }

            std::string describe() const override {
                return "unordered elements are " +
                       ::Catch::Detail::stringify( m_desired );
            }
        };

        template<typename T>
        struct MatcherEquals : MatcherGenericBase {

            template<typename T2>
            MatcherEquals(T2&& desired)
                : m_desired( CATCH_FORWARD( desired ) ) {
            }

            template<typename T3>
            bool match( T3&& arg ) const {
                return arg == m_desired;
            }

            std::string describe() const override {
                return "object match ";
            }
        private:
            T m_desired;
        };

        template <typename... TargetTypes>
        class ElementsMatchMatchersMatcher final
            : public MatcherGenericBase {

        public:
            template <typename... TargetTypes2>
            ElementsMatchMatchersMatcher( TargetTypes2&&... args ) {
                // Map the arguments into the matcher factories
                constexpr int size = sizeof...(args) + 1;
                int dummy[size] = { ( store_matcher(
                                          CATCH_FORWARD( args ) ),
                                      0 )... };

                // We don't need this, but we also can't omit it
                (void) dummy;
            }

            template <typename ElementType>
            void const* store_matcher(ElementType&& element) {

                m_matchers.push_back( std::make_unique<MatcherEquals<ElementType>>(element));
                return m_matchers.back().get();
            }

            template <typename RangeLike>
            bool match( RangeLike&& rng ) const {

                size_t mi = 0;
                for (const auto& elem : rng) {
                    // ran out of target matchers
                    if (mi >= m_matchers.size()) {
                        return false;
                    }

                    // HACK: silence error while I figure this out
                    (void) elem;

                    // TODO: figure out how to actual call this function
                    // if (!m_matchers[mi].match(elem)) {
                    //     return false;
                    // }
                    
                    ++mi;
                }
                return true;
            }

            std::string describe() const override {
                return "elements match " ;//+
                    //    ::Catch::Detail::stringify( m_desired );
            }

        private:
            std::vector<std::unique_ptr<MatcherGenericBase>> m_matchers;
        };

        /**
         * Creates a matcher that checks if all elements in a range are equal
         * to all elements in another range.
         *
         * Uses `std::equal_to` to do the comparison
         */
        template <typename RangeLike>
        std::enable_if_t<!Detail::is_matcher<RangeLike>::value,
                         ElementsAreElementsMatcher<RangeLike, std::equal_to<>>>
        ElementsAre( RangeLike range ) {
            return { CATCH_FORWARD( range ), std::equal_to<>{} };
        }

        /**
         * Creates a matcher that checks if all elements in a range are equal
         * to all elements in another range.
         *
         * Uses to provided predicate `predicate` to do the comparisons
         */
        template <typename RangeLike, typename Equality>
        ElementsAreElementsMatcher<RangeLike, Equality>
        ElementsAre( RangeLike&& range, Equality&& predicate ) {
            return { CATCH_FORWARD( range ), CATCH_FORWARD( predicate ) };
        }

        template<typename... Ts>
        ElementsMatchMatchersMatcher<Ts...>
        // ElementsAreElementsMatcher<std::vector<int>, std::equal_to<>>
        ElementsAreA( Ts&&... desired ) {
            return {desired...};
        }

        /**
         * Creates a matcher that checks if all elements in a range are equal
         * to all elements in another range, in some permutation
         *
         * Uses `std::equal_to` to do the comparison
         */
        template <typename RangeLike>
        std::enable_if_t<
            !Detail::is_matcher<RangeLike>::value,
            UnorderedElementsAreElementsMatcher<RangeLike, std::equal_to<>>>
        UnorderedElementsAre( RangeLike range ) {
            return { CATCH_FORWARD( range ), std::equal_to<>{} };
        }

        /**
         * Creates a matcher that checks if all elements in a range are equal
         * to all elements in another range, in some permuation.
         *
         * Uses to provided predicate `predicate` to do the comparisons
         */
        template <typename RangeLike, typename Equality>
        UnorderedElementsAreElementsMatcher<RangeLike, Equality>
        UnorderedElementsAre( RangeLike&& range, Equality&& predicate ) {
            return { CATCH_FORWARD( range ), CATCH_FORWARD( predicate ) };
        }
    } // namespace Matchers
} // namespace Catch

#endif // CATCH_MATCHERS_ELEMENTS_ARE_HPP_INCLUDED
