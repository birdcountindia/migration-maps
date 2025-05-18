# Animated migration maps

These migration maps are GIFs of various migratory bird species, showing how their distribution across the globe changes across seasons. They are extremely popular and a great way of engaging audiences with data on bird migration. 

First, unfortunately, all the individual species data files (for the whole world) need to be downloaded manually and saved in the appropriate folder. After this, the workflow is automated so the pipeline script can be run directly.

## Current status

As of 2025-05-18, coding has been completed for the base point map animation and for the smoothed line inset animation. The two have been combined in a patchwork, but [animating them together is a pending task](https://github.com/birdcountindia/migration-maps/issues/6). The remaining image elements to be overlayed are simple, but [their positioning needs to be coded and finalised](https://github.com/birdcountindia/migration-maps/issues/5). There are a [couple other immediate pending tasks](https://github.com/birdcountindia/migration-maps/issues?q=is%3Aissue%20state%3Aopen%20label%3Apending), some [optimisations required](https://github.com/birdcountindia/migration-maps/issues?q=is%3Aissue%20state%3Aopen%20label%3Aoptimisation), and [other tasks for when the main pipeline is finalised](https://github.com/birdcountindia/migration-maps/issues?q=is%3Aissue%20state%3Aopen%20label%3Afinal-leg).
