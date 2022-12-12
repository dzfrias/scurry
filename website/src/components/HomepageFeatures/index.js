import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Component Based',
    description: (
      <>
        Scurry protects state with an object system that encourages small,
        reusable components.
      </>
    ),
  },
  {
    title: 'Intuitively Designed',
    description: (
      <>
        Scurry's syntax is meant to be as intuitive as possible. There are no
        surprises, or extraneous syntax.
      </>
    ),
  },
  {
    title: 'Simple',
    description: (
      <>
        There is a very obvious way to write programs in Scurry. By keeping
        things simple, programs acutally <strong>work</strong>!
      </>
    ),
  },
];

function Feature({title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center padding-horiz--md">
        <h2>{title}</h2>
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
