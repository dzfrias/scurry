import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Component Based',
    Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        Scurry protects state with an object system that encourages small,
        reusable components.
      </>
    ),
  },
  {
    title: 'Intuitively Designed',
    Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Scurry's syntax is meant to be as intuitive as possible. There are no
        surprises, or extraneous syntax.
      </>
    ),
  },
  {
    title: 'Simple',
    Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        There is a very obvious way to write programs in Scurry. By keeping
        things simple, programs acutally <strong>work</strong>!
      </>
    ),
  },
];

function Feature({Svg, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
