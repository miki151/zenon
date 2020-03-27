//
// Copyright (c) 2016-2018 Martin Moene
//
// https://github.com/martinmoene/any-lite
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#pragma once

#ifndef NONSTD_ANY_LITE_HPP
#define NONSTD_ANY_LITE_HPP


#include <utility>


namespace nonstd {  

using std::remove_reference;

namespace detail {

// for any_REQUIRES_T

/*enum*/ class enabler{};

} // namespace detail

class any
{
public:
    any() noexcept
    : content( nullptr )
    {}

    any( any && other ) noexcept
    : content( std::move( other.content ) )
    {
        other.content = nullptr;
    }

    template<
        class ValueType, class T = typename std::decay<ValueType>::type
    >
    any( ValueType && value ) noexcept
    : content( new holder<T>( std::forward<ValueType>( value ) ) )
    {}

    ~any()
    {
        reset();
    }

    any & operator=( any && other ) noexcept
    {
        content = other.content;
        other.content = nullptr;
        return *this;
    }

    template<
        class ValueType, class T = typename std::decay<ValueType>::type
    >
    any & operator=( ValueType && value )
    {
        content = new holder<T>( std::forward<ValueType>( value ) );
        return *this;
    }

    void reset() noexcept
    {
        delete content; content = nullptr;
    }

    void swap( any & other ) noexcept
    {
        std::swap( content, other.content );
    }

    void* get_ptr() const
    {
        return content->get_ptr();
    }

private:
    class placeholder
    {
    public:
        virtual ~placeholder()
        {
        }

        virtual void* get_ptr() const = 0;
    };

    template< typename ValueType >
    class holder : public placeholder
    {
    public:
        holder( ValueType const & value )
        : held( value )
        {}

        holder( ValueType && value )
        : held( std::move( value ) )
        {}

        virtual void* get_ptr() const override
        {
            return (void*)&held;
        }

        ValueType held;
    };

    placeholder * content;
};

} // namespace nonstd

#endif // NONSTD_ANY_LITE_HPP
