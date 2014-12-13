# Responsive Image Module for Zotonic

Automatically generate images at the right size for each screen dimension.

## Usage

### Zotonic template

In a template where the image should be responsive:

    <img data-media_id="{{ media_id }}" data-range="320-1280" />


### Javascript

    $.responsive({
        urlSource: "/responsive_image"
    });
    $("img").responsive();


See [Fetch Responsive Image jQuery plugin](https://github.com/ArthurClemens/jquery-fetch-responsive-plugin) for extensive documentation of the jQuery plugin.

Note that when a high resolution image is requested, Zotonic image attribute `upscale` is set to true.


### Zotonic specific parameters

#### urlSource

(required) 

This is always `"/responsive_image"`.


#### media_id

(required) 

Id of image resource.

#### mediaclass

This module takes over the image sizing, but you can still use other attributes set in a mediaclass config file.

    {"article",
        [
            {magick, "+level-colors green,gold"},
            {blur, "20x8"},
            {quality, 85},
            {extent, false}
        ]
    }
    
And then:

    <img data-media_id="{{ media_id }}" data-mediaclass="article" />

### crop

Set this attribute to a true value to activate cropping. Possible values:

* 0/false: no cropping
* 1/true: crops using default crop (center)
* "auto": crops using the crop center (if any is set, otherwise no cropping)
* any cropping value (north, north_east, east, south_east, south, south_west, west, north_west and center)

Example:

    <img data-media_id="{{ media_id }}" data-crop="auto" />

or:

    $("img").responsive({
        crop: "auto"
    });

### Other image attributes

* background
* extent
* blur
* flip
* flop
* grey
* lossless
* mono
* quality
* upscale
* use_absolute_url

## Complete example

### Template

    {% with id.o.depiction.id as media_id %}
        {% if media_id %}
            <div class="main-image">
                <img data-media_id="{{ media_id }}" data-mediaclass="article" data-crop="1" data-high-resolution="auto" data-high-resolution-maximum="1200"  data-ratio="3.2" data-range="320-1280" data-use_absolute_url="1" data-blur="12x20" />
            </div>
        {% endif %}
    {% endwith %}
    
### Javascript

    $.responsive({
        urlSource: "/responsive_image"
    });
    $(".main-image img").responsive();

## Installation

* Download and activate this module in Admin > System > Modules.
* Download [Fetch Responsive Image jQuery plugin](https://github.com/ArthurClemens/jquery-fetch-responsive-plugin).
* Place Javascript file `jquery.fetch-responsive.min.js` in a lib folder.
* Link the Javascript file in a template where images are loaded, or in base.tpl.
